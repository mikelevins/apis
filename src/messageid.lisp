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
;;; Apis uses these 64-bit integer values as message identifiers. Each
;;; sets *next-message-id* to 0 and creates a singleton agent that
;;; dispenses monotonically-increasing messageids.
