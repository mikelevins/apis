;;;; ***********************************************************************
;;;;
;;;; Name:          worker.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       worker implementation 
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defparameter *next-worker-id* 0)

(defun get-next-worker-id ()
  (let ((next (incf *next-worker-id*)))
    next))

(defclass worker ()
  ((id :reader worker-id :initform (get-next-worker-id) :initarg :id)
   (message-queue :accessor worker-message-queue :initform (make-instance 'queues:simple-cqueue))
   (event-process :accessor worker-event-process :initform nil)))


;;; each worker binds this special in its event thread
(defparameter *current-worker* nil)
(defun current-worker () *current-worker*)

(defmethod initialize-instance :after ((worker worker)
                                       &rest initargs
                                       &key &allow-other-keys)
  (register-worker worker)
  )

(defmethod start-worker ((worker worker))
  )

(defmethod stop-worker ((worker worker))
  )

#+nil (defparameter $w1 (make-instance 'worker))
#+nil (register-worker $w1)
#+nil (describe $w1)

