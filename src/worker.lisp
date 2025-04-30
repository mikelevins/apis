;;;; ***********************************************************************
;;;;
;;;; Name:          worker.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       worker implementation
;;;; Author:        mikel evins
;;;; Copyright:     2015-2025 by mikel evins
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
   (message-semaphore :accessor worker-message-semaphore :initform (bt:make-semaphore :name "message semaphore") )
   (message-thread :accessor worker-message-thread :initform nil :initarg :message-thread)))

#+repl (defparameter $w1 (make-instance 'worker))
#+repl (describe $w1)
#+repl (integer-length (worker-id $w1))
#+repl (time (loop for i from 0 below 1000000 do (makeid)))

(defmethod receive ((worker worker)(msg message))
  (format t "~% worker ~S received message ~S" worker msg))

(defmethod start-worker ((worker worker) &key thread-name)
  (let* ((thread-name (or thread-name (format nil "worker-thread (~X)" (worker-id worker))))
         (thread (bt:make-thread
                  (lambda ()
                    (loop ; loop forever
                          (bt:wait-on-semaphore (worker-message-semaphore worker))
                          (loop ; loop over the message queue
                                for msg = (queues:qpop (worker-message-queue worker))
                                while msg
                                do (receive worker msg))))
                  :name thread-name)))
    (setf (worker-message-thread worker) thread)))

(defmethod stop-worker ((worker worker))
  (let ((thread (shiftf (worker-message-thread worker) nil)))
    (when thread
      (bt:destroy-thread thread))))

#+repl (defparameter $w1 (make-instance 'worker))
#+repl (describe $w1)
#+repl (start-worker $w1)
#+repl (send (message :from t :to $w1))
#+repl (stop-worker $w1)
