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
   (message-data :reader envelope-message-data :initform nil :initarg :message-data)))

(defmethod print-object ((obj envelope) out-stream)
  (print-unreadable-object (obj out-stream :type t :identity t)
    (format out-stream "destination-host: ~S, destination-port: ~S message-data: ~S"
            (envelope-destination-host obj)
            (envelope-destination-port obj)
            (envelope-message-data obj))))

