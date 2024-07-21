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
  (;; a vector of 20 bytes
   (id :reader message-id :initform (ksuid:make-ksuid) :initarg :id :type 'ksuid:ksuid)
   ;; a delivery-address
   (from :reader message-from :initform nil :initarg :from :type (or null delivery-address))
   ;; a delivery-address
   (to :reader message-to :initform nil :initarg :to :type (or null delivery-address))
   ;; a keyword
   (operation :reader message-operation :initform :ping :initarg :operation :type (or null symbol))
   (arguments :reader message-arguments :initform nil :initarg :arguments)
   (timestamp :reader message-timestamp :initform (local-time:now) :initarg :timestamp)
   (time-to-live :reader message-time-to-live :initform *default-message-time-to-live* :initarg :time-to-live)))

(defmethod print-object ((obj message) out-stream)
  (print-unreadable-object (obj out-stream :type t :identity nil)
    (format out-stream "~S ~S"
            (ksuid:ksuid->string (message-id obj))
            (message-operation obj))))


(defmethod message-id-number ((msg message))
  (ksuid:ksuid->integer (message-id msg)))

(defmethod message-id-string ((msg message))
  (ksuid:ksuid->string (message-id msg)))


#+nil (defparameter $msg1 (make-instance 'message
                                         :from nil
                                         :to (delivery-address)))
#+nil (describe $msg1)
#+nil (describe (bytes->object (object->bytes $msg1)))
