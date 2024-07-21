;;;; tests.lisp

(in-package #:apis)

;;; identifiers
;;; ---------------------------------------------------------------------

#+nil (time (ksuid:make-ksuid))
#+nil (ksuid:ksuid->string (ksuid:make-ksuid))

;;; serialization
;;; ---------------------------------------------------------------------

#+nil (type-of (object->bytes (list :list 1 "two" 3)))
#+nil (bytes->object (object->bytes (list :list 1 "two" 3)))


;;; workers, message delivery
;;; ---------------------------------------------------------------------

#+nil (defparameter $w1-id (coerce #(19 43 49 22 209 197 130 168 120 139 61 59 207 89 75 132 142 158 239 170)
                                   'ksuid:ksuid))
#+nil (defparameter $w1
        (make-instance 'worker :description "test worker 1"
                       :id $w1-id))
#+nil (bt:threadp (worker-message-thread $w1))
#+nil (start-worker $w1)
#+nil (stop-worker $w1)

#+nil (defparameter $msg1 (make-instance 'message :to (delivery-address :worker $w1) :operation :ping))
#+nil (send $msg1)

#+nil (defparameter $jupiter "192.168.0.64")
#+nil (defparameter $saturn "192.168.0.159")
#+nil (defparameter $msg2 (make-instance 'message
                                         :to (delivery-address :host $saturn :port *message-receive-port* :worker $w1-id)
                                         :operation :ping))
#+nil (describe (the-relayer))
#+nil (start-relayer (the-relayer))
#+nil (stop-relayer (the-relayer))
#+nil (send $msg2)



#+nil (setf $bytes (object->bytes $e1))
#+nil (length $bytes)
#+nil (setf $e2 (bytes->object $bytes))
#+nil (message $e2)

#+nil (defparameter $msg3 (make-instance 'message :to nil :operation :ping))
#+nil (send $msg3)
#+nil (describe (elt *dead-messages* 0))

#+nil *dead-messages*
