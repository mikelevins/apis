;;;; ***********************************************************************
;;;;
;;;; Name:          service-worker.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       a worker that continuously runs some code besides its message thread
;;;; Author:        mikel evins
;;;; Copyright:     2015-2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defclass service-worker (worker)
  ((service-thread :accessor service-thread :initform nil :initarg :service-thread)))

(defmethod start ((worker service-worker) &key thread-name service-function &allow-other-keys)
  (call-next-method)
  ;; TODO: add code to start the service thread
  )

(defmethod stop ((worker service-worker))
  (call-next-method)
  ;; TODO: add code to stop the service thread
  )

#+repl (defparameter $s1 (make-instance 'service-worker))
#+repl (describe $s1)
#+repl (start $s1)
#+repl (send (message :from t :to $s1))
#+repl (stop $s1)
