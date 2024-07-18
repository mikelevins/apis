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

(defparameter *dead-messages* (make-array 32 :initial-element nil :fill-pointer 0 :adjustable t))

(defmethod file-dead-message ((message message))
  (warn "Filing a dead message: ~S" message)
  (vector-push-extend message *dead-messages* 16))

(defun get-local-ips ()
  (let* ((local-interfaces (ip-interfaces:get-ip-interfaces))
         (local-ips (mapcar (lambda (intf)(ip-interfaces:ip-interface-address intf))
                            local-interfaces)))
    (loop for ip in local-ips
          collect (format nil "~A.~A.~A.~A"
                          (elt ip 0)
                          (elt ip 1)
                          (elt ip 2)
                          (elt ip 3)))))

#+nil (get-local-ips)

(defmethod localhost-address? ((addr string))
  (or (and (equal "localhost" addr) t)
      (and (member addr (get-local-ips) :test #'equalp) t)))

#+nil (localhost-address? "127.0.0.1")
#+nil (localhost-address? "192.168.0.64")

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
            (not (localhost-address? (host to-address))))
       (file-dead-message message))
      ;; it's addressed to a worker by ID-string
      ((stringp (worker to-address))(let ((receiver (identify-worker (worker to-address))))
                                      (if receiver
                                          (deliver-locally message receiver)
                                          (file-dead-message message))))
      ;; the receiver is not the dispatcher or an identifiable worker
      (t (file-dead-message message)))))

(defmethod handle-received-operation ((worker dispatcher) (msg message)(op symbol))
  (format t "~%The apis dispatcher received a ~S message: ~S" op msg))


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
                                                               :worker (worker-id-string $w1))
                                         :operation :ping))
#+nil (localhost-address? (host (message-to $msg1)))
#+nil (worker (message-to $msg1))
#+nil (dispatch-message $msg1)
