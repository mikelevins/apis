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

(defmethod receive ((env envelope))
  (let* ((worker (identify-worker (to-worker env)))
         (op (message-operation (message env))))
    (handle-received-operation worker env op)))

;;; ---------------------------------------------------------------------
;;; GENERIC FUNCTION handle-message-operation worker message op
;;; ---------------------------------------------------------------------

;;; unrecognized operations
;;; ---------------------------------------------------------------------

;;; :ping and :ack
;;; ---------------------------------------------------------------------

(defmethod handle-received-operation ((worker worker) (env envelope)(op (eql :ping)))
  (format t "~%~S received :ping" worker))

