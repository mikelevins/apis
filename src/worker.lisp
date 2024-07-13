;;;; ***********************************************************************
;;;;
;;;; Name:          worker.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       worker implementation
;;;; Author:        mikel evins
;;;; Copyright:     2015-2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; CLASS worker
;;; ---------------------------------------------------------------------
;;; abstract superclass of all workers

;;; TODO: arrange to at intervals check the messages-waiting-for-reply
;;;       and discard those whose time-to-live has expired. also, when
;;;       discarding an expired message, signal a condition.

(defclass worker ()
  ((id :reader worker-id :initform (makeid) :initarg :id)
   (name :initform nil :initarg :name)
   (messages-waiting-for-reply :initform (make-hash-table) :initarg :messages-waiting-for-reply)))

(defmethod worker-name ((worker worker))
  (or (slot-value worker 'name)
      (let ((class-name (class-name (class-of worker))))
        (intern (format nil "~A-~X" class-name (worker-id worker))
                :keyword))))

(defmethod print-object ((obj worker) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~A" (worker-name obj))))

;;; ---------------------------------------------------------------------
;;; CLASS local-worker
;;; ---------------------------------------------------------------------
;;; a worker whose thread runs in the local process

(defclass local-worker (worker)
  ((id :reader worker-id :initform (makeid) :initarg :id)
   (name :initform nil :initarg :name)
   (message-queue :accessor worker-message-queue :initform (make-instance 'queues:simple-cqueue))
   (message-thread :accessor worker-message-thread :initform nil :initarg :message-thread)
   (message-semaphore :accessor worker-message-semaphore :initform (bt:make-semaphore :name "message semaphore") )))

(defmethod handle-message-operation ((worker local-worker) (msg message) op)
  (format t "~%Worker ~S received: ~S ~S~%" worker
          (message-operation msg)
          (message-arguments msg)))

(defmethod handle-message-operation ((worker local-worker) (msg message)(op (eql :ping)))
  (format t "~%Worker ~S received :ping" worker))

(defmethod handle-message ((worker local-worker) msg)
  (format t "Worker ~S received unrecognized message: ~S"
          worker (with-output-to-string (s)
                   (describe msg s))))

(defmethod handle-message ((worker local-worker) (msg message))
  (let ((op (message-operation msg))
        (args (message-arguments msg)))
    (handle-message-operation worker msg op)))

(defmethod make-worker-message-thread ((worker local-worker) &key thread-name)
  (bt:make-thread
   (lambda ()
     (loop ; loop forever
      (bt:wait-on-semaphore (worker-message-semaphore worker))
      (loop ; loop over the message queue
            for msg = (queues:qpop (worker-message-queue worker))
            while msg
            do (handle-message worker msg))))
   :name (or thread-name (format nil "message thread [~A]" worker))))

(defmethod start-worker ((worker local-worker))
  (unless (bt:threadp (worker-message-thread worker))
    (setf (worker-message-thread worker)
          (make-worker-message-thread worker))))

(defmethod stop-worker ((worker local-worker))
  (let ((thread (shiftf (worker-message-thread worker) nil)))
    (when thread
      (bt:destroy-thread thread))))

(defmethod worker-running? ((worker local-worker))
  (and (worker-message-thread worker) t))

(defmethod send-message (msg (worker local-worker))
  (let ((q (worker-message-queue worker)))
    (bt:with-recursive-lock-held ((queues::lock-of q))
      (queues:qpush q msg)
      (bt:signal-semaphore (worker-message-semaphore worker)))))

;;; ---------------------------------------------------------------------
;;; CLASS remote-worker
;;; ---------------------------------------------------------------------
;;; a worker whose thread runs in some other process, possibly on
;;; another host

(defclass remote-worker ()
  ((host :initform nil :initarg :host)
   (port :initform nil :initarg :port)))


