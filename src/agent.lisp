;;;; ***********************************************************************
;;;;
;;;; Name:          agent.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       agent implementation 
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defclass agent ()
  ((id :reader agent-id :initform (make-session-id) :initarg :id)
   (name :initform nil :initarg :name)
   (message-queue :accessor agent-message-queue :initform (make-instance 'queues:simple-cqueue))
   (event-process :accessor agent-event-process :initform nil)))

#+nil (defparameter $a1 (make-instance 'agent))
#+nil (describe $a1)
