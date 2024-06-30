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


(defclass messenger (worker)
  ((workers :accessor workers :initform nil)
   (receive-port :accessor messenger-receive-port :initform nil)
   (receive-socket :accessor messenger-receive-socket :initform nil)
   (receiver-process :accessor messenger-receiver-process :initform nil)
   (receive-queue :accessor messenger-receive-queue :initform nil)
   (receive-buffer :accessor messenger-receive-buffer :initform nil)
   (sender-process :accessor messenger-sender-process :initform nil)
   (send-queue :accessor messenger-send-queue :initform nil)
   (send-buffer :accessor messenger-send-buffer :initform nil))
  (:metaclass singleton-class))

(defun the-messenger ()
  (make-instance 'messenger))

#+nil (describe (the-messenger))

(defun find-registered-worker (worker-id)
  (gethash worker-id
           (workers (the-messenger))
           nil))

(defmethod register-worker ((worker worker))
  (let ((already (find-registered-worker (worker-id worker))))
    (if already
        (error "A worker with ID ~A is already registered." (worker-id worker))
        (setf (gethash (worker-id worker) (workers (the-messenger)))
              worker))))

#+nil (defparameter $w (make-instance 'worker))
#+nil (register-worker $w)

(defmethod unregister-worker ((worker worker))
  (let ((found (find-registered-worker (worker-id worker))))
    (if found
        (progn (stop-worker found)
               (remhash (worker-id worker)
                        (workers (the-messenger))))
        (setf (gethash (worker-id worker)
                       (workers (the-messenger)))
              worker))))

(defun start-messaging ()
  (unless (messenger-send-queue (the-messenger))
    (setf (messenger-send-queue (the-messenger))
          (make-instance 'simple-cqueue)))
  (unless (messenger-receive-queue (the-messenger))
    (setf (messenger-receive-queue (the-messenger))
          (make-instance 'simple-cqueue)))
  (start-worker (the-messenger)))

(defun stop-messaging ()
  (stop-worker (the-messenger))
  (setf (messenger-send-queue (the-messenger)) nil)
  (setf (messenger-receive-queue (the-messenger)) nil))

(defun reset-the-messenger ()
  (stop-messaging)
  (reset-singleton-class (find-class 'messenger))
  (start-messaging))

#+nil (describe (the-messenger))
#+nil (reset-the-messenger)
#+nil (register-worker (make-instance 'worker))

(defun deliver-message-locally (message)
  (let ((destination (destination-worker message)))
    (if (or (eq destination (the-messenger))
            (null destination))
        (qpush (messenger-receive-queue (the-messenger)) message)
        (qpush (worker-message-queue destination) message))))

(defun deliver-message-remotely (message)
  (format t "Not yet implemented: remote delivery ~S" message))

(defmethod send-message ((message message))
  (let ((destination-host (destination-host message)))
    (if (member destination-host '(nil "localhost" "127.0.0.1") :test 'equal)
        (deliver-message-locally message)
        (deliver-message-remotely message))))

#+nil (stop-messaging)
#+nil (start-messaging)
#+nil (send-message (message :destination-worker (the-messenger)))
#+nil (describe (the-messenger))
#+nil (describe (messenger-receive-queue (the-messenger)))
