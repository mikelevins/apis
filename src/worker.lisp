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

(defclass worker ()
  ((id :reader worker-id :initform (makeid) :initarg :id)
   (description :reader worker-description :initform nil :initarg :description)
   (message-queue :accessor worker-message-queue :initform (make-instance 'queues:simple-cqueue))
   (message-thread :accessor worker-message-thread :initform nil :initarg :message-thread)
   (message-semaphore :accessor worker-message-semaphore :initform (bt:make-semaphore :name "message semaphore") )
   (messages-waiting-for-reply :initform (make-hash-table) :initarg :messages-waiting-for-reply)))

(defmethod workerp (thing) nil)
(defmethod workerp ((thing worker)) t)

(defmethod print-object ((obj worker) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~S" (or (worker-description obj)
                                   (worker-id obj)))))

(defmethod make-worker-message-thread ((worker worker) &key thread-name)
  (bt:make-thread
   (lambda ()
     (loop ; loop forever
      (bt:wait-on-semaphore (worker-message-semaphore worker))
      (loop ; loop over the message queue
            for msg = (queues:qpop (worker-message-queue worker))
            while msg
            do (handle-message worker msg))))
   :name (or thread-name (format nil "message thread [~A]" worker))))

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

(defmethod send-message (msg (worker worker))
  (let ((q (worker-message-queue worker)))
    (bt:with-recursive-lock-held ((queues::lock-of q))
      (queues:qpush q msg)
      (bt:signal-semaphore (worker-message-semaphore worker)))))



