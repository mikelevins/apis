;;;; ***********************************************************************
;;;;
;;;; Name:          messenger.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       message transport
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)


(defclass messenger ()
  ((receive-port :accessor messenger-receive-port :initform nil)
   (receive-socket :accessor messenger-receive-socket :initform nil)
   (receiver-process :accessor messenger-receiver-process :initform nil)
   (receive-queue :accessor messenger-receive-queue :initform nil)
   (receive-buffer :accessor messenger-receive-buffer :initform nil)
   (local-delivery-process :accessor messenger-local-delivery-process :initform nil)
   (sender-process :accessor messenger-sender-process :initform nil)
   (send-queue :accessor messenger-send-queue :initform nil)
   (send-buffer :accessor messenger-send-buffer :initform nil))
  (:metaclass singleton-class))

(defun the-messenger ()
  (make-instance 'messenger))

(defun reset-the-messenger ()
  (reset-singleton-class (find-class 'messenger)))

#+nil (describe (the-messenger))
#+nil (reset-the-messenger)
