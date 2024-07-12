;;;; ***********************************************************************
;;;;
;;;; Name:          message.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       representation of messages
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; message
;;; ---------------------------------------------------------------------


(defclass message ()
  ((id :reader message-id :initform (makeid) :initarg :id)
   (from :reader message-from :initform nil :initarg :from)
   (to :reader message-to :initform nil :initarg :to)
   (operation :reader message-operation :initform :ping :initarg :operation)
   (arguments :reader message-arguments :initform nil :initarg :arguments)
   (timestamp :reader message-timestamp :initform (local-time:now) :initarg :timestamp)
   (time-to-live :reader message-time-to-live :initform *default-message-time-to-live* :initarg :time-to-live)))

(defmethod print-object ((obj message) out-stream)
  (print-unreadable-object (obj out-stream :type t :identity nil)
    (format out-stream "~S ~S"
            (message-id obj)
            (message-operation obj))))

#+nil (defparameter $msg1 (make-instance 'message))
#+nil (describe $msg1)
#+nil (describe (bytes->object (object->bytes $msg1)))

