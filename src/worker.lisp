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
  ((id :reader id :initform (makeid) :initarg :id)
   (description :reader description :initform nil :initarg :description)
   (message-queue :accessor message-queue :initform (make-instance 'queues:simple-cqueue))
   (message-semaphore :accessor message-semaphore :initform (bt:make-semaphore :name "message semaphore") )
   (message-thread :accessor message-thread :initform nil :initarg :message-thread)))

#+repl (defparameter $w1 (make-instance 'worker))
#+repl (describe $w1)
#+repl (integer-length (id $w1))
#+repl (time (loop for i from 0 below 1000000 do (makeid)))

(defmethod receive ((worker worker)(msg message))
  (format t "~% worker ~S received message ~S" worker msg))

(defmethod start ((worker worker) &key thread-name &allow-other-keys)
  (let* ((thread-name (or thread-name (format nil "thread (~X)" (id worker))))
         (thread (bt:make-thread
                  (lambda ()
                    (loop ; loop forever
                          (bt:wait-on-semaphore (message-semaphore worker))
                          (loop ; loop over the message queue
                                for msg = (queues:qpop (message-queue worker))
                                while msg
                                do (receive worker msg))))
                  :name thread-name)))
    (setf (message-thread worker) thread)))

(defmethod stop ((worker worker))
  (let ((thread (shiftf (message-thread worker) nil)))
    (when thread
      (bt:destroy-thread thread))))

#+repl (defparameter $w1 (make-instance 'worker))
#+repl (describe $w1)
#+repl (start $w1)
#+repl (send (message :from t :to $w1))
#+repl (stop $w1)
