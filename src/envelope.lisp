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
   ;; the name of a known worker object available at the detination host and port
   (destination-worker :reader envelope-destination-worker :initform nil :initarg :destination-worker)
   (contents :reader envelope-contents :initform nil :initarg :contents)))

(defmethod print-object ((obj envelope) out-stream)
  (print-unreadable-object (obj out-stream :type t :identity nil)
    (format out-stream "to: ~S@~A:~A"
            (envelope-destination-worker obj)
            (envelope-destination-host obj)
            (envelope-destination-port obj))))

