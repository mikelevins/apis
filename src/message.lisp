;;;; ***********************************************************************
;;;;
;;;; Name:          message.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       representation of messages
;;;; Author:        mikel evins
;;;; Copyright:     2023 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.apis)

;;; ---------------------------------------------------------------------
;;; message
;;; ---------------------------------------------------------------------
;;; the data transmitted from agent to agent

(defparameter *default-message-time-to-live* 600) ; seconds

(defclass message ()
  ((operation :reader message-operation :initform :ping :initarg :operation)
   (arguments :reader message-arguments :initform nil :initarg :arguments)
   (timestamp :reader message-timestamp :initform (local-time:now) :initarg :timestamp)
   (time-to-live :reader message-time-to-live :initform *default-message-time-to-live* :initarg :time-to-live)
   (nodeid :reader message-nodeid :initform (this-nodeid) :initarg :nodeid)
   (sessionid :reader message-sessionid :initform (get-session-id) :initarg :sessionid)
   (messageid :reader message-id :initform (next-message-id) :initarg :id)
   (destination-host :reader destination-host :initform nil :initarg :destination-host)
   (destination-port :reader destination-port :initform nil :initarg :destination-port)
   (destination-agent :reader destination-agent :initform nil :initarg :destination-agent)))

(defmethod print-object ((obj message) out-stream)
  (print-unreadable-object (obj out-stream :type t :identity nil)
    (let ((timestamp (message-timestamp obj)))
      (format out-stream "~A: ~S ~S"
              (message-id obj)
              (message-operation obj)
              (message-arguments obj)))))

#+nil (defparameter $msg1 (make-instance 'message))
