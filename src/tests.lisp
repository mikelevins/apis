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
#+nil (list-local-workers)
#+nil (find-local-worker $w1)
#+nil (find-local-worker (worker-id-string $w1))
#+nil (bt:threadp (worker-message-thread $w1))
#+nil (start-worker $w1)
#+nil (stop-worker $w1)

#+nil (defparameter $msg1 (make-instance 'message :operation :ping))
#+nil (setf $e1 (make-instance 'envelope :to-worker $w1 :message $msg1))
#+nil (send $e1 $w1)

#+nil (defparameter $msg2
        (make-instance 'message :operation :foo
                       :arguments '(:bar :baz)))

#+nil (setf $bytes (object->bytes $e1))
#+nil (length $bytes)
#+nil (setf $e2 (bytes->object $bytes))
#+nil (message $e2)


#+nil (send $e1 $w1)
#+nil (time (queues:qtop (worker-message-queue $w1)))
#+nil (time (queues:qsize (worker-message-queue $w1)))

