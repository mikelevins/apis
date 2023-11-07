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
;;; the data transmitted from agent to agent

(defparameter *default-message-time-to-live* 600) ; seconds

(defclass message ()
  ((sender-agent-id :reader message-sender-agent-id :initform nil :initarg :sender-agent-id)
   (destination-agent-id :reader message-destination-agent-id :initform nil :initarg :destination-agent-id)
   (data :reader message-data :initform nil :initarg :data)
   (timestamp :reader message-timestamp :initform (get-universal-time) :initarg :timestamp)
   (time-to-live :reader message-time-to-live :initform *default-message-time-to-live* :initarg :time-to-live)))

(defmethod print-object ((obj message) out-stream)
  (print-unreadable-object (obj out-stream :type t :identity nil)
    (let ((timestamp (message-timestamp obj)))
      (format out-stream "timestamp: ~S"
              (if timestamp
                  (local-time:universal-to-timestamp timestamp)
                  nil)))))


;;; a singleton message is a one-shot, asynchronous, fire-and-forget message
;;; once sent, it's forgotten. best used for updates that can safely be
;;; lost.
(defclass singleton-message (message)
  ((id :reader message-id :initform (makeid) :initarg :id)))


;;; a protocol message is an element of a protocol exchange. a protocol exchange
;;; requires a certain specified sequence of messages between participants.
;;; each agent maintains a log of protocol messages sent and received so that
;;; if a message is dropped it can be resent, and so that when messages
;;; arrive out of order they can be properly reordered.
(defclass protocol-message (message)
  ((protocol :reader message-protocol :initform nil :initarg :protocol)
   (session-id :reader message-session-id :initform (makeid) :initarg :session-id)
   (sequence-number :reader message-sequence-number :initform nil :initarg :sequence-number)))


