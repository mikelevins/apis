;;;; ***********************************************************************
;;;;
;;;; Name:          dispatcher.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       local delivery of remote messages
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; dead (undeliverable) messages
;;; ---------------------------------------------------------------------

(defparameter *dead-messages* (make-array 32 :initial-element nil :fill-pointer 0 :adjustable t))

(defmethod file-dead-message ((message message))
  (log-message (format nil "Filing a dead message: ~S" message))
  (vector-push-extend message *dead-messages* 16))

;;; ---------------------------------------------------------------------
;;; SINGLETON CLASS dispatcher
;;; ---------------------------------------------------------------------

(defclass dispatcher (worker)
  ()
  (:metaclass singleton-class))

(defmethod make-worker-message-thread ((dispatcher dispatcher) &key thread-name)
  (bt:make-thread
   (lambda ()
     (loop ; loop forever
      (bt:wait-on-semaphore (worker-message-semaphore dispatcher))
      (loop ; loop over the message queue
            for msg = (queues:qpop (worker-message-queue worker))
            while msg
            do (dispatch-message msg))))
   :name (or thread-name (format nil "message thread" worker))))

(defun the-dispatcher ()
  (make-instance 'dispatcher))

(defun reset-the-dispatcher ()
  (reset-singleton-class (find-class 'dispatcher)))

;;; find the local worker that is the recipient of the message and deliver it
(defmethod dispatch-message ((message message))
  (let* ((to-address (message-to message)))
    (cond
      ;; it's addressed to the dispatcher
      ((or (null to-address)
           (eq (the-dispatcher)
               (worker to-address)))
       (handle-received-operation (the-dispatcher) message (message-operation message)))

      ;; the host isn't local
      ((and (not (null (host to-address)))
            (not (localhost-ip-address? (host to-address))))
       (file-dead-message message))

      ;; it's addressed to a worker by ID
      ((typep (worker to-address) 'ksuid:ksuid)(let ((receiver (identify-worker (worker to-address))))
                                                 (if receiver
                                                     (deliver-locally message receiver)
                                                     (file-dead-message message))))
      ;; the receiver is not the dispatcher or an identifiable worker
      (t (file-dead-message message)))))


#+nil (eq (the-dispatcher)(the-dispatcher))
#+nil (reset-the-dispatcher)
#+nil (start-worker (the-dispatcher))
#+nil (stop-worker (the-dispatcher))
#+nil (defparameter $w1 (make-instance 'worker :description "test worker 1"))
#+nil (start-worker $w1)
#+nil (stop-worker $w1)
#+nil (defparameter $msg1 (make-instance 'message
                                         :to (delivery-address :host "192.168.0.64"
                                                               :port *message-receive-port*
                                                               :worker (worker-id $w1))
                                         :operation :ping))
#+nil (localhost-ip-address? (host (message-to $msg1)))
#+nil (worker (message-to $msg1))
#+nil (dispatch-message $msg1)
