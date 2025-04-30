;;;; ***********************************************************************
;;;;
;;;; Name:          recipient.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       a thing that can receive a message
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :apis)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; a recipient is something to which we can deliver a message. For now
;;; it must be a worker. Later we may add remote delivery, in which case
;;; it will also be allowed to be whatever proxy we use for workers on
;;; remote processes.

(deftype recipient () worker)
