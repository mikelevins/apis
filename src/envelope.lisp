;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
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


