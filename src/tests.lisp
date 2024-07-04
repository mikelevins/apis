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

#+nil (send-message $msg1 "192.168.0.51" *message-receive-port*)
#+nil (send-message $msg1 "192.168.0.159" *message-receive-port*)
#+nil (send-message $msg2 "192.168.0.159" *message-receive-port*)
#+nil (describe (messenger-receive-queue (the-messenger)))
#+nil (describe (messenger-send-queue (the-messenger)))
#+nil (describe (the-messenger))
#+nil (stop-messaging)


;;; workers, message delivery
;;; ---------------------------------------------------------------------

#+nil (start-messaging)
#+nil (stop-messaging)
#+nil (defparameter $a (make-instance 'worker))
#+nil (defparameter $b (make-instance 'worker))
#+nil (describe $a)
#+nil (describe $b)
#+nil (start-worker $a)
#+nil (start-worker $b)
#+nil (deliver-message-to-worker (vector 1) $a)
#+nil (deliver-message-to-worker (vector 2) $b)
#+nil (deliver-message-to-worker (vector 1 2) $a)
#+nil (deliver-message-to-worker (vector 1 2 3) $a)
#+nil (stop-worker $a)
#+nil (stop-worker $b)
#+nil (queues:qsize (worker-message-queue $a))
#+nil (queues:qsize (worker-message-queue $b))
#+nil (list-known-workers)
#+nil (list-running-workers)
#+nil (list-stopped-workers)
