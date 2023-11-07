;;;; ***********************************************************************
;;;;
;;;; Name:          envelope.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       routing wrapper for message data
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; envelope
;;; ---------------------------------------------------------------------

(defclass envelope ()
  ((destination-host :reader envelope-destination-host :initform nil :initarg :destination-host)
   (destination-port :reader envelope-destination-port :initform nil :initarg :destination-port)
   ;; the name of a known agent object available at the detination host and port
   (destination-agent :reader envelope-destination-agent :initform nil :initarg :destination-agent)
   (message-data :reader envelope-message-data :initform nil :initarg :message-data)))

(defmethod print-object ((obj envelope) out-stream)
  (print-unreadable-object (obj out-stream :type t :identity nil)
    (format out-stream "~A:~S, agent: ~S, data: ~S"
            (envelope-destination-host obj)
            (envelope-destination-port obj)
            (envelope-destination-agent obj)
            (envelope-message-data obj))))

