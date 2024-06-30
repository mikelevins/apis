;;;; ***********************************************************************
;;;;
;;;; Name:          sessionid.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       64-bit per-process integer IDs for Apis sessions
;;;; Author:        mikel evins
;;;; Copyright:     2023 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.apis)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; Apis uses these 64-bit integer values as session identifiers. Each
;;; time Apis starts it creates a sessionid whose higher-order 32 bits
;;; are the value of (get-universal-time) and whose lower-order 32 bits
;;; are a random integer in the range #x0-#xffffffff.

(defparameter *session-random-state* (make-random-state t))

(defun make-session-id ()
  (let* ((high-bits (get-universal-time))
         (low-bits (random #xffffffff *session-random-state*)))
    (+ low-bits (ash high-bits 32))))

(let ((*session-id* (make-session-id)))
  (defun get-session-id ()
    *session-id*))

#+nil (integer-length (get-session-id))
