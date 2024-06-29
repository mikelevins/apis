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

(defun start-messaging ()
  (setf (messenger-receive-queue (the-messenger))
        (make-instance 'simple-cqueue)))

(defun stop-messaging ()
  (setf (messenger-receive-queue (the-messenger)) nil))

(defun reset-the-messenger ()
  (stop-messaging)
  (reset-singleton-class (find-class 'messenger))
  (start-messaging))

#+nil (describe (the-messenger))
#+nil (reset-the-messenger)

(defun deliver-message-locally (message)
  (let ((destination (destination-agent message)))
    (if (or (null destination)
            (eq destination (the-messenger)))
        (qpush (messenger-receive-queue (the-messenger)) message)
        (qpush (agent-message-queue destination) message))))

(defun deliver-message-remotely (message)
  (format t "Not yet implemented: remote delivery ~S" message))

(defmethod send-message ((message message))
  (let ((destination-host (destination-host message)))
    (if (member destination-host '(nil "localhost" "127.0.0.1") :test 'equal)
        (deliver-message-locally message)
        (deliver-message-remotely message))))

#+nil (stop-messaging)
#+nil (start-messaging)
#+nil (send-message (message :destination-agent (the-messenger)))
