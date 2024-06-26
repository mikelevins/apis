;;;; ***********************************************************************
;;;;
;;;; Name:          messageid.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       64-bit per-device, per-session integer IDs for messages
;;;; Author:        mikel evins
;;;; Copyright:     2023 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.apis)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; Apis uses these 64-bit integer values as message identifiers.
;;; Each session sets *next-message-id* to zero at startup, then
;;; increments and returns an id each time one is requested.

(let ((*next-message-id* 0))
  (defun last-message-id ()
    *next-message-id*)
  (defun next-message-id ()
    (incf *next-message-id*))
  (defun reset-message-id ()
    (setf *next-message-id* 0)
    (values)))

#+nil (next-message-id)
