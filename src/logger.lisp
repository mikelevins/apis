;;;; ***********************************************************************
;;;;
;;;; Name:          logger.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       logging messages and informational output
;;;; Author:        mikel evins
;;;; Copyright:     2015-2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defparameter *print-logged-messages* t)

(defclass logger ()
  ((messages :accessor logger-messages :initform (make-array 16 :fill-pointer 0 :adjustable t)))
  (:metaclass singleton-class))

(defun the-logger ()
  (make-instance 'logger))

(defmethod log-message ((msg string))
  (when *print-logged-messages*
    (format t "~%~A" msg)))

(defmethod log-message ((msg message))
  (when *print-logged-messages*
    (format t "~% Logging message: ~S" msg))
  (vector-push-extend msg (logger-messages (the-logger))))
