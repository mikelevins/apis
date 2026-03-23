;;;; ***********************************************************************
;;;;
;;;; Name:          runtime.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       runtime scheduling, send, and worker registry
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; CLASS runtime
;;; ---------------------------------------------------------------------

(defclass runtime ()
  ((threads :accessor runtime-threads :initform nil)
   (thread-count :reader runtime-thread-count
                 :initform *default-runtime-thread-count*
                 :initarg :thread-count :type integer)
   (ready-queue :accessor runtime-ready-queue
                :initform (make-instance 'queues:simple-cqueue))
   (ready-lock :reader runtime-ready-lock
               :initform (bt:make-lock "ready-lock"))
   (ready-condvar :reader runtime-ready-condvar
                  :initform (bt:make-condition-variable :name "ready-condvar"))
   (registry :reader runtime-registry
             :initform (make-hash-table :test 'eql))
   (registry-lock :reader runtime-registry-lock
                  :initform (bt:make-lock "registry-lock"))
   (running-p :accessor runtime-running-p :initform nil :type boolean)))

(defmethod (setf runtime-thread-count) (new-count (runtime runtime))
  "Set the thread count. Declined with a warning if the runtime is running."
  (cond ((runtime-running-p runtime)
         (warn "Cannot change thread count while the runtime is running.")
         (runtime-thread-count runtime))
        (t (setf (slot-value runtime 'thread-count) new-count))))

(defmethod print-object ((rt runtime) out-stream)
  (print-unreadable-object (rt out-stream :type t :identity nil)
    (format out-stream "~D thread~:P ~A"
            (runtime-thread-count rt)
            (if (runtime-running-p rt) "[running]" "[stopped]"))))

(defun make-runtime (&key (thread-count *default-runtime-thread-count*))
  (make-instance 'runtime :thread-count thread-count))

;;; ---------------------------------------------------------------------
;;; registry operations
;;; ---------------------------------------------------------------------

(defun register-worker (worker runtime)
  "Register a worker in the runtime's registry. Thread-safe."
  (bt:with-lock-held ((runtime-registry-lock runtime))
    (setf (gethash (worker-id worker) (runtime-registry runtime)) worker)))

(defun find-worker (id runtime)
  "Look up a worker by ULID in the runtime's registry. Thread-safe."
  (bt:with-lock-held ((runtime-registry-lock runtime))
    (gethash id (runtime-registry runtime))))

;;; ---------------------------------------------------------------------
;;; dead letters
;;; ---------------------------------------------------------------------

(defun file-dead-letter (message reason)
  "Record an undeliverable message in *dead-letters*."
  (let ((entry (cons reason message)))
    (vector-push-extend entry *dead-letters* 16)))

;;; ---------------------------------------------------------------------
;;; scheduling loop
;;; ---------------------------------------------------------------------

(defun scheduling-loop (runtime)
  "The function executed by each runtime scheduling thread.
Loops: pull a ready worker, let it process one message,
return it to the ready-queue or mark it idle."
  (loop
    (let ((worker nil))
      ;; wait for a ready worker or shutdown
      (bt:with-lock-held ((runtime-ready-lock runtime))
        (loop while (and (runtime-running-p runtime)
                         (zerop (queues:qsize (runtime-ready-queue runtime))))
              do (bt:condition-wait (runtime-ready-condvar runtime)
                                   (runtime-ready-lock runtime)))
        (unless (runtime-running-p runtime)
          (return))
        (setf worker (queues:qpop (runtime-ready-queue runtime)))
        (setf (worker-state worker) :running))
      ;; process one message outside the lock
      (when worker
        (let ((msg (queues:qpop (worker-message-queue worker))))
          (when msg
            (handler-case (receive worker msg)
              (error (err)
                (warn "Error processing message for worker ~S: ~A" worker err)))))
        ;; update worker state
        (bt:with-lock-held ((runtime-ready-lock runtime))
          (cond ((plusp (queues:qsize (worker-message-queue worker)))
                 (setf (worker-state worker) :ready)
                 (queues:qpush (runtime-ready-queue runtime) worker)
                 (bt:condition-notify (runtime-ready-condvar runtime)))
                (t (setf (worker-state worker) :idle))))))))

;;; ---------------------------------------------------------------------
;;; start and stop
;;; ---------------------------------------------------------------------

(defun start-runtime (runtime)
  "Spawn scheduling threads and begin processing messages."
  (unless (runtime-running-p runtime)
    (setf (runtime-running-p runtime) t)
    (setf (runtime-threads runtime)
          (loop for i from 1 to (runtime-thread-count runtime)
                collect (bt:make-thread
                         (lambda () (scheduling-loop runtime))
                         :name (format nil "apis scheduler ~D" i)))))
  runtime)

(defun stop-runtime (runtime)
  "Shut down all scheduling threads. Blocks until all threads have exited.
Workers and their queued messages are preserved."
  (when (runtime-running-p runtime)
    (setf (runtime-running-p runtime) nil)
    ;; wake all threads so they see running-p is false
    (bt:with-lock-held ((runtime-ready-lock runtime))
      (dotimes (i (runtime-thread-count runtime))
        (bt:condition-notify (runtime-ready-condvar runtime))))
    ;; join all threads
    (dolist (thread (runtime-threads runtime))
      (when (bt:threadp thread)
        (bt:join-thread thread)))
    (setf (runtime-threads runtime) nil))
  runtime)

(defun clear-all-queues (runtime)
  "Drain all worker message queues and the ready-queue, resetting every
worker to :idle. Declined with a warning if the runtime is running."
  (when (runtime-running-p runtime)
    (warn "Cannot clear queues while the runtime is running.")
    (return-from clear-all-queues nil))
  ;; drain each worker's message queue and reset state
  (maphash (lambda (id worker)
             (declare (ignore id))
             (loop while (plusp (queues:qsize (worker-message-queue worker)))
                   do (queues:qpop (worker-message-queue worker)))
             (setf (worker-state worker) :idle))
           (runtime-registry runtime))
  ;; drain the ready-queue
  (loop while (plusp (queues:qsize (runtime-ready-queue runtime)))
        do (queues:qpop (runtime-ready-queue runtime)))
  runtime)

;;; ---------------------------------------------------------------------
;;; send
;;; ---------------------------------------------------------------------

(defgeneric send (message)
  (:documentation "Deliver a message to the worker named by its TO field.
If the target worker is not found, the message is filed as a dead letter."))

(defmethod send ((msg message))
  (let* ((runtime *default-runtime*)
         (target-id (message-to msg))
         (worker (when (and runtime target-id)
                   (find-worker target-id runtime))))
    (cond
      ((null runtime)
       (file-dead-letter msg "No runtime available."))
      ((null worker)
       (file-dead-letter msg (format nil "Worker ~A not found."
                                     (if target-id (format-id target-id) "nil"))))
      (t
       ;; push onto the worker's message queue (thread-safe)
       (queues:qpush (worker-message-queue worker) msg)
       ;; if idle, transition to ready and wake a scheduling thread
       (bt:with-lock-held ((runtime-ready-lock runtime))
         (when (eql (worker-state worker) :idle)
           (setf (worker-state worker) :ready)
           (queues:qpush (runtime-ready-queue runtime) worker)
           (bt:condition-notify (runtime-ready-condvar runtime))))))))

;;; ---------------------------------------------------------------------
;;; default runtime
;;; ---------------------------------------------------------------------

(setf *default-runtime* (make-runtime :thread-count *default-runtime-thread-count*))
