;;;; ***********************************************************************
;;;;
;;;; Name:          worker.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       worker implementation for abcl
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; *worker-lock*
;;; ---------------------------------------------------------------------
;;; controls concurrent access to workers

(defparameter *worker-lock* (bt:make-lock))

;;; ---------------------------------------------------------------------
;;; CLASS worker
;;; ---------------------------------------------------------------------
;;; represents workers and worker processes

(defclass worker ()
  ((id :reader worker-id :initform (makeid) :initarg :id)
   (name :initform nil :initarg :name)
   (message-ready? :reader worker-message-ready? :initform (bt:make-condition-variable))
   (message-queue :accessor worker-message-queue :initform (make-instance 'queues:simple-cqueue))
   (event-process :accessor worker-event-process :initform nil)))

(defmethod worker-name ((worker worker))
  (or (slot-value worker 'name)
      (worker-id worker)))

(defmethod print-object ((obj worker) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~A" (worker-name obj))))

(defmethod handle-message-operation ((worker worker) (msg message) op)
  (format t "~%Worker ~S received message:~%  ~S~%" worker msg))

(defmethod handle-message-operation ((worker worker) (msg message)(op (eql :ping)))
  (format t "~%Worker ~S received :ping" worker))

(defmethod handle-message ((worker worker) (msg message))
  (let ((op (message-operation msg)))
    (handle-message-operation worker msg op)))

(defmethod handle-message ((worker worker) msg)
  (format t "Worker ~S received unrecognized message: ~S"
            worker (with-output-to-string (s)
                    (describe msg s))))

(defmethod make-worker-event-process ((worker worker))
  (bt:make-thread
   (lambda ()
     (loop
        (bt:with-lock-held (*worker-lock*)
          (loop for msg = (queues:qpop (worker-message-queue worker))
             then (queues:qpop (worker-message-queue worker))
             while msg
             do (handle-message worker msg))
          (bt:condition-wait (worker-message-ready? worker)
                             *worker-lock*))))
   :name (format nil "worker-event-process [~A]" worker)))

(defmethod start-worker ((worker worker))
  (setf (worker-event-process worker)
        (make-worker-event-process worker)))

(defmethod stop-worker ((worker worker))
  (let ((handler (shiftf (worker-event-process worker) nil)))
    (when handler
      (bt:destroy-thread handler))))

(defmethod worker-running? ((worker worker))
  (and (worker-event-process worker) t))

(defmethod deliver-message-to-worker (msg (worker worker))
  (bt:with-lock-held (*worker-lock*)
    (queues:qpush (worker-message-queue worker) msg)
    (bt:condition-notify (worker-message-ready? worker))))


;;; ---------------------------------------------------------------------
;;; SINGLETON-CLASS known-workers
;;; ---------------------------------------------------------------------
;;; keeps track of known workers for messaging and management purposes

(defclass known-workers ()
  ((roster :reader known-workers-roster :initform (make-hash-table :test 'eql)))
  (:metaclass singleton-class))

(defun the-known-workers ()(make-instance 'known-workers))

(defmethod find-known-worker ((name symbol))
  (gethash name (known-workers-roster (the-known-workers)) nil))

(defun list-known-workers ()
  (loop for key being the hash-keys in (known-workers-roster (the-known-workers))
     collect key))

(defun list-running-workers ()
  (let ((found nil))
    (loop for key being the hash-keys in (known-workers-roster (the-known-workers))
       using (hash-value val)
       do (when (worker-running? val)
            (pushnew key found)))
    found))

(defun list-stopped-workers ()
  (let ((found nil))
    (loop for key being the hash-keys in (known-workers-roster (the-known-workers))
       using (hash-value val)
       do (when (not (worker-running? val))
            (pushnew key found)))
    found))

(defmethod define-known-worker ((name symbol)(worker worker))
  (let ((old-worker (find-known-worker name)))
    (when old-worker (stop-worker old-worker)))
  (setf (gethash name (known-workers-roster (the-known-workers)))
        worker)
  worker)

(defmethod remove-known-worker ((name symbol))
  (let ((old-worker (find-known-worker name)))
    (when old-worker (stop-worker old-worker)))
  (remhash name (known-workers-roster (the-known-workers)))
  name)
