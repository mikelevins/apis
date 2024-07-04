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
   (operation :reader message-operation :initform :ping :initarg :operation)
   (arguments :reader message-arguments :initform nil :initarg :arguments)
   (timestamp :reader message-timestamp :initform (local-time:now) :initarg :timestamp)
   (time-to-live :reader message-time-to-live :initform *default-message-time-to-live* :initarg :time-to-live)
   (to-host :reader message-to-host :initform nil :initarg :to-host)
   (to-port :reader message-to-port :initform nil :initarg :to-port)
   (to-worker :reader message-to-worker :initform nil :initarg :to-worker)))

(defmethod print-object ((obj message) out-stream)
  (print-unreadable-object (obj out-stream :type t :identity nil)
    (let ((timestamp (message-timestamp obj)))
      (format out-stream "~S ~S"
              (message-id obj)
              (message-operation obj)))))

#+nil (defparameter $msg1 (make-instance 'message))
#+nil (describe $msg1)
#+nil (describe (bytes->object (object->bytes $msg1)))

