;;;; ***********************************************************************
;;;;
;;;; Name:          receive.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       handling received envelopes and messages
;;;; Author:        mikel evins
;;;; Copyright:     2015-2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; GENERIC FUNCTION receive worker envelope
;;; ---------------------------------------------------------------------

(defmethod receive ((msg message))
  (let* ((to-address (message-to msg))
         (worker (identify-worker (worker to-address)))
         (op (message-operation msg)))
    (handle-received-operation worker msg op)))

;;; ---------------------------------------------------------------------
;;; GENERIC FUNCTION handle-message-operation worker message op
;;; ---------------------------------------------------------------------

;;; unrecognized operations
;;; ---------------------------------------------------------------------

;;; :ping and :ack
;;; ---------------------------------------------------------------------

(defmethod handle-received-operation ((worker worker) (msg message)(op (eql :ping)))
  (format t "~%~S received :ping" worker))

