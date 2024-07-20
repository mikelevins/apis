;;;; tests.lisp

(in-package #:apis)

;;; identifiers
;;; ---------------------------------------------------------------------

#+nil (time (ksuid::make-ksuid))

;;; serialization
;;; ---------------------------------------------------------------------

#+nil (type-of (object->bytes (list :list 1 "two" 3)))
#+nil (bytes->object (object->bytes (list :list 1 "two" 3)))


;;; workers, message delivery
;;; ---------------------------------------------------------------------

#+nil (defparameter $w1 (make-instance 'worker :description "test worker 1"))
#+nil (bt:threadp (worker-message-thread $w1))
#+nil (start-worker $w1)
#+nil (stop-worker $w1)

#+nil (defparameter $msg1 (make-instance 'message :to (delivery-address :worker $w1) :operation :ping))
#+nil (describe $msg1)
#+nil (describe (message-to $msg1))
#+nil (send $msg1)

#+nil (defparameter $jupiter "192.168.0.64")
#+nil (defparameter $saturn "192.168.0.159")
#+nil (defparameter $msg2 (make-instance 'message
                                         :to (delivery-address :host $saturn :worker "2jLVM21qFXkITB5Ro48Ac2gf9bM")
                                         :operation :ping))
#+nil (describe $msg2)
#+nil (send $msg2)



#+nil (setf $bytes (object->bytes $e1))
#+nil (length $bytes)
#+nil (setf $e2 (bytes->object $bytes))
#+nil (message $e2)


