;;;; tests.lisp

(in-package #:apis)

;;; identifiers
;;; ---------------------------------------------------------------------

#+nil (get-ip-address)
#+nil (nodebits)
#+nil (timestamp-milliseconds)
#+nil (time (makeid))
#+nil (integer-length (makeid))


;;; messenger
;;; ---------------------------------------------------------------------

#+nil (type-of (object->bytes (list :list 1 "two" 3)))
#+nil (bytes->object (object->bytes (list :list 1 "two" 3)))
#+nil (start-messaging)
#+nil (defparameter $msg1 (make-instance 'singleton-message :operation nil :data '(1 2 3)))
#+nil (defparameter $msg2 (make-instance 'singleton-message :operation :ping))
#+nil (send-message $msg1 *localhost* *message-receive-port*)
#+nil (send-message $msg2 *localhost* *message-receive-port*)
#+nil (send-message $msg1 *localhost* *message-receive-port* :default-recipient)

#+nil (send-message $msg1 "192.168.0.51" *message-receive-port*)
#+nil (send-message $msg2 "192.168.0.159" *message-receive-port*)
#+nil (describe (messenger-receive-queue (the-messenger)))
#+nil (describe (messenger-send-queue (the-messenger)))
#+nil (describe (the-messenger))
#+nil (stop-messaging)


;;; agents and message delivery
;;; ---------------------------------------------------------------------

#+nil (start-messaging)
#+nil (stop-messaging)
#+nil (defparameter $a (make-instance 'agent))
#+nil (defparameter $b (make-instance 'agent))
#+nil (describe $a)
#+nil (describe $b)
#+nil (start-agent $a)
#+nil (start-agent $b)
#+nil (deliver-message-to-agent (vector 1) $a)
#+nil (deliver-message-to-agent (vector 2) $b)
#+nil (deliver-message-to-agent (vector 1 2) $a)
#+nil (deliver-message-to-agent (vector 1 2 3) $a)
#+nil (stop-agent $a)
#+nil (stop-agent $b)
#+nil (queues:qsize (agent-message-queue $a))
#+nil (queues:qsize (agent-message-queue $b))
#+nil (list-known-agents)
#+nil (list-running-agents)
#+nil (list-stopped-agents)
