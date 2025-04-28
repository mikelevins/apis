;;;; ***********************************************************************
;;;;
;;;; Name:          worker.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       worker implementation
;;;; Author:        mikel evins
;;;; Copyright:     2015-2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; CLASS worker
;;; ---------------------------------------------------------------------

(defun make-worker-id (&key time random-integer)
  (+ (ash (or time (get-universal-time)) 32)
     (or random-integer (random #xFFFFFFFF *id-random-state*))))

(defclass worker ()
  ((id :reader worker-id :initform (make-worker-id) :initarg :id)
   (description :reader worker-description :initform nil :initarg :description)
   (message-queue :accessor worker-message-queue :initform (make-instance 'queues:simple-cqueue))
   (message-thread :accessor worker-message-thread :initform nil :initarg :message-thread)
   (message-semaphore :accessor worker-message-semaphore :initform (bt:make-semaphore :name "message semaphore") )
   (messages-waiting-for-reply :initform (make-hash-table) :initarg :messages-waiting-for-reply)))

#+test (defparameter $w1 (make-instance 'worker))
#+test (integer-length (worker-id $w1))
#+test (time (loop for i from 0 below 1000000 do (make-worker-id)))
