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

(defparameter *local-workers* (make-hash-table :test 'equalp))

(defun make-worker-id (&key time random-integer)
  (cons (or time (get-universal-time))
        (or random-integer (random most-positive-fixnum *id-random-state*))))

(defclass worker ()
  ((id :reader worker-id :initform (make-worker-id) :initarg :id)
   (description :reader worker-description :initform nil :initarg :description)
   (message-queue :accessor worker-message-queue :initform (make-instance 'queues:simple-cqueue))
   (message-thread :accessor worker-message-thread :initform nil :initarg :message-thread)
   (message-semaphore :accessor worker-message-semaphore :initform (bt:make-semaphore :name "message semaphore") )
   (messages-waiting-for-reply :initform (make-hash-table) :initarg :messages-waiting-for-reply)))

#+test (defparameter $w1 (make-instance 'worker))
