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


;;; workers, message delivery
;;; ---------------------------------------------------------------------

#+nil (defparameter $w1 (make-instance 'local-worker))
#+nil (worker-name $w1)
#+nil (bt:threadp (worker-message-thread $w1))
#+nil (start-worker $w1)
#+nil (stop-worker $w1)

#+nil (defparameter $msg1
        (make-instance 'message :operation :ping
                       :from nil
                       :to $w1))

#+nil (defparameter $msg2
        (make-instance 'message :operation :foo
                       :arguments '(:bar :baz)
                       :from nil
                       :to $w1))

#+nil (send-message $msg1 $w1)
#+nil (send-message $msg2 $w1)
#+nil (time (queues:qtop (worker-message-queue $w1)))
#+nil (time (queues:qsize (worker-message-queue $w1)))
#+nil (send-message $msg2 $w1)
#+(nil (loop for i from 0 below 5 do (send-message $msg1 $w1)))
#+(nil (loop for i from 0 below 5 do (send-message $msg2 $w1)))

