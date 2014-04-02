;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
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

(defparameter *default-message-time-to-live* 600) ; seconds

(defclass message ()
  ((id :reader message-id :initform (makeid) :initarg :id)
   (sender-id :accessor sender-id :initform nil :initarg :sender-id)
   (sender-host :accessor sender-host :initform nil :initarg :sender-host)
   (sender-port :accessor sender-port :initform nil :initarg :sender-port)
   (destination-id :accessor destination-id :initform nil :initarg :destination-id)
   (destination-host :accessor destination-host :initform nil :initarg :destination-host)
   (destination-port :accessor destination-port :initform nil :initarg :destination-port)
   (in-response-to :reader in-response-to :initform nil :initarg :in-response-to)
   (timestamp :reader message-timestamp :initform (get-universal-time) :initarg :timestamp)
   (time-to-live :reader message-time-to-live :initform *default-message-time-to-live* :initarg :time-to-live)))

(defun message (message-class-name &rest initargs)
  (apply #'make-instance message-class-name initargs))

(defmethod encode-message ((message message))
  (ccl:with-output-to-vector (out)
    (cl-store:store message out)))

(defmethod decode-message ((message-data vector))
  (ccl:with-input-from-vector (in message-data)
    (cl-store:restore in)))

;;; general requests

(defclass version-request (message)())

(defclass status-request (message)())

;;; general responses

(defclass version-response (message)
  ((version :reader get-version :initarg :version)))

(defclass status-response (message)
  ((status :reader get-status :initarg :status)))


