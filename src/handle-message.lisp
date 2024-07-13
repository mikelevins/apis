;;;; ***********************************************************************
;;;;
;;;; Name:          handle-message.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       methods for handling received messages
;;;; Author:        mikel evins
;;;; Copyright:     2015-2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; GENERIC FUNCTION handle-message worker message
;;; ---------------------------------------------------------------------

(defmethod handle-message ((worker worker) (msg message))
  (let ((op (message-operation msg))
        (args (message-arguments msg)))
    (handle-message-operation worker msg op)))

;;; ---------------------------------------------------------------------
;;; GENERIC FUNCTION handle-message-operation worker message op
;;; ---------------------------------------------------------------------

;;; unrecognized operations
;;; ---------------------------------------------------------------------

(defmethod handle-message-operation ((worker worker) (msg message) op)
  (format t "~%~S received: ~S ~S" worker
          (message-operation msg)
          (message-arguments msg)))

;;; :ping and :ack
;;; ---------------------------------------------------------------------

(defmethod handle-message-operation ((worker worker) (msg message)(op (eql :ping)))
  (format t "~%~S received :ping" worker))

