;;;; ***********************************************************************
;;;;
;;;; Name:          dead-messages.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       messages that cannot be delivered
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defparameter *dead-messages* (make-array 32 :initial-element nil :fill-pointer 0 :adjustable t))

(defclass dead-message ()
  ((contents :accessor dead-message-contents :initform nil :initarg :contents)
   (explanation :accessor dead-message-explanation :initform nil :initarg :explanation)))

(defmethod dead-message ((message message)(explanation string))
  (make-instance 'dead-message :contents message :explanation explanation))

(defmethod file-dead-message ((message message) &key (explanation "Message delivery failed."))
  (let ((dm (dead-message message (format nil "Dead message: ~A; ~A"
                                          (message-id-string message)
                                          explanation))))
    (log-message (format nil "Filing a dead message: ~S" message))
    (vector-push-extend dm *dead-messages* 16)))
