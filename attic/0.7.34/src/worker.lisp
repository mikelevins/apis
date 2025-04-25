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

(defparameter *local-workers* (make-hash-table :test 'equalp))

(defclass worker ()
  ((id :reader worker-id :initform (ksuid:make-ksuid) :initarg :id)
   (description :reader worker-description :initform nil :initarg :description)
   (message-queue :accessor worker-message-queue :initform (make-instance 'queues:simple-cqueue))
   (message-thread :accessor worker-message-thread :initform nil :initarg :message-thread)
   (message-semaphore :accessor worker-message-semaphore :initform (bt:make-semaphore :name "message semaphore") )
   (messages-waiting-for-reply :initform (make-hash-table) :initarg :messages-waiting-for-reply)))

(defmethod initialize-instance :after ((instance worker) &rest initargs &key &allow-other-keys)
  (let* ((id (worker-id instance))
         (already-worker (gethash id *local-workers* nil)))
    (assert (null already-worker)() "There is already a worker with ID ~S" id)
    (setf (gethash id *local-workers*) instance)))

(defmethod worker-id-number ((worker worker))
  (ksuid:ksuid->integer (worker-id worker)))

(defmethod worker-id-string ((worker worker))
  (ksuid:ksuid->string (worker-id worker)))

(defmethod workerp (thing) nil)
(defmethod workerp ((thing worker)) t)

(defmethod print-object ((obj worker) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~X" (ksuid:ksuid->string (worker-id obj)))
    (when (worker-description obj)
      (format stream " ~S" (worker-description obj)))
    (when (and (worker-message-thread obj)
               (bt:thread-alive-p (worker-message-thread obj)))
      (format stream " [RUNNING]"))))

(defun list-local-worker-ids ()
  (loop for k being the hash-keys in *local-workers*
        collect k))

(defun list-local-workers ()
  (loop for v being the hash-values in *local-workers*
        collect v))

(defmethod make-worker-message-thread ((worker worker) &key thread-name)
  (bt:make-thread
   (lambda ()
     (loop ; loop forever
      (bt:wait-on-semaphore (worker-message-semaphore worker))
      (loop ; loop over the message queue
            for msg = (queues:qpop (worker-message-queue worker))
            while msg
            do (receive msg))))
   :name (or thread-name (format nil "message thread [~A]" (worker-id-string worker)))))

(defmethod start-worker ((worker worker))
  (unless (bt:threadp (worker-message-thread worker))
    (setf (worker-message-thread worker)
          (make-worker-message-thread worker))))

(defmethod stop-worker ((worker worker))
  (let ((thread (shiftf (worker-message-thread worker) nil)))
    (when thread
      (bt:destroy-thread thread))))

(defmethod worker-running? ((worker worker))
  (and (worker-message-thread worker) t))

(defmethod identify-worker ((thing worker))
  thing)

(defmethod identify-worker ((thing vector))
  (gethash thing *local-workers* nil))

(defmethod identify-worker ((thing string))
  (gethash (ksuid:string->ksuid thing) *local-workers* nil))

(defmethod identify-worker ((thing integer))
  (gethash (ksuid:integer->ksuid thing) *local-workers* nil))
