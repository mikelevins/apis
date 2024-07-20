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

;;; BUG: need to make the postoffice's queues thread-safe

(defmethod send ((message message))
  (let ((to-address (message-to message)))
    (if (local-address? to-address)
        (deliver-locally message)
        (deliver-remotely message))))

(defmethod deliver-locally ((message message))
  (let ((addr (message-to message)))
    (if (delivery-address? addr)
        (let ((worker (identify-worker (worker addr))))
          (if worker
              (let ((q (worker-message-queue worker)))
                (bt:with-recursive-lock-held ((queues::lock-of q))
                  (queues:qpush q message)
                  (bt:signal-semaphore (worker-message-semaphore worker))))
              (file-dead-message message)))
        (file-dead-message message))))

(defmethod deliver-remotely ((message message))
  (queues:qpush (postoffice-send-queue (the-postoffice)) 
                message))


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

(defmethod handle-received-operation ((worker null) (msg message)(op symbol))
  (log-message (format nil "~%The apis process received a ~S message: ~S" op msg)))

;;; :ping and :ack
;;; ---------------------------------------------------------------------

(defmethod handle-received-operation ((worker worker) (msg message)(op (eql :ping)))
  (log-message (format nil "~%~S received :ping" worker)))

