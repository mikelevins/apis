;;;; tests.lisp

(in-package #:apis)

;;; identifiers
;;; ---------------------------------------------------------------------

#+nil (time (makeid))
#+nil (integer-length (makeid))

;;; serialization
;;; ---------------------------------------------------------------------

#+nil (type-of (object->bytes (list :list 1 "two" 3)))
#+nil (bytes->object (object->bytes (list :list 1 "two" 3)))


;;; messenger, local delivery
;;; ---------------------------------------------------------------------

#+nil (start-messaging)
#+nil (defparameter $msg1 (make-instance 'message :operation nil :arguments '(1 2 3)
                                         :to-host *localhost* :to-port *message-receive-port*
                                         :to-worker :default-recipient))
#+nil (defparameter $msg2 (make-instance 'message :operation :ping
                                         :to-host *localhost* :to-port *message-receive-port*
                                         :to-worker :default-recipient))
#+nil (send-message $msg1)
#+nil (send-message $msg2)

;;; messenger, remote delivery
;;; ---------------------------------------------------------------------
;;; destinations:
;;; - jupiter: "192.168.0.64"
;;; - saturn: "192.168.0.159"

;;; to jupiter
#+nil (defparameter $msg1 (make-instance 'message :operation nil :arguments '(1 2 3)
                                         :to-host "192.168.0.64" :to-port *message-receive-port*
                                         :to-worker :default-recipient))
;;; to jupiter
#+nil (defparameter $msg2 (make-instance 'message :operation :ping
                                         :to-host "192.168.0.64" :to-port *message-receive-port*
                                         :to-worker :default-recipient))
;;; to saturn
#+nil (defparameter $msg3 (make-instance 'message :operation nil :arguments '(1 2 3)
                                         :to-host "192.168.0.159" :to-port *message-receive-port*
                                         :to-worker :default-recipient))

#+nil (send-message $msg1)
#+nil (send-message $msg2)
#+nil (send-message $msg3)

#+nil (stop-messaging)


;;; workers, message delivery
;;; ---------------------------------------------------------------------

#+nil (defparameter $w1 (make-instance 'worker))
#+nil (worker-name $w1)
#+nil (bt:threadp (worker-message-thread $w1))
#+nil (start-worker $w1)
#+nil (stop-worker $w1)

#+nil (defparameter $msg1
        (make-instance 'message :operation :ping
                       :from nil
                       :to $w1))

#+nil (deliver-message $msg1 $w1)
#+nil (time (queues:qtop (worker-message-queue $w1)))
#+nil (time (queues:qsize (worker-message-queue $w1)))
#+nil (deliver-message $msg2 $w1)
#+(nil (loop for i from 0 below 5 do (deliver-message $msg1 $w1)))
#+(nil (loop for i from 0 below 5 do (deliver-message $msg2 $w1)))

