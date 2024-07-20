;;;; ***********************************************************************
;;;;
;;;; Name:          send-receive.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       sending and receiving messages
;;;; Author:        mikel evins
;;;; Copyright:     2015-2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; GENERIC FUNCTION send message 
;;; ---------------------------------------------------------------------

(defmethod send ((message message))
  (let ((to-address (message-to message)))
    (if (local-address? to-address)
        (let ((worker (worker to-address)))
          (deliver-locally message worker))
        (deliver-remotely message))))

;;; ---------------------------------------------------------------------
;;; GENERIC FUNCTION receive message
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

(defmethod handle-received-operation ((worker dispatcher) (msg message)(op symbol))
  (log-message (format nil "~%The apis dispatcher received a ~S message: ~S" op msg)))

;;; :ping and :ack
;;; ---------------------------------------------------------------------

(defmethod handle-received-operation ((worker worker) (msg message)(op (eql :ping)))
  (log-message (format nil "~%~S received :ping" worker)))

