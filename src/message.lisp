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
  ((id :accessor message-id :initform (makeid) :initarg :id)
   (operation :accessor message-operation :initform :ping :initarg :operation)
   (arguments :accessor message-arguments :initform nil :initarg :arguments)
   (timestamp :accessor message-timestamp :initform (local-time:now) :initarg :timestamp)
   (time-to-live :accessor message-time-to-live :initform *default-message-time-to-live* :initarg :time-to-live)
   (recipient :accessor message-recipient :initform nil :initarg :recipient)))

(defmethod print-object ((obj message) out-stream)
  (print-unreadable-object (obj out-stream :type t :identity nil)
    (let ((timestamp (message-timestamp obj)))
      (format out-stream "~X ~S"
              (message-id obj)
              (message-operation obj)))))

#+nil (setf $local (local-recipient))
#+nil (defparameter $msg1 (make-instance 'message :recipient $local))
#+nil (describe $msg1)
#+nil (describe (bytes->object (object->bytes $msg1)))

