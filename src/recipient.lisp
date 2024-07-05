;;;; ***********************************************************************
;;;;
;;;; Name:          recipient.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       local and remote worker identifiers for message delivery
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defclass recipient ()())

;;; recipients for workers in the same Lisp process
(defclass local-recipient ()
  ((worker :reader worker :initform nil :initarg :worker)))

(defun ensure-valid-local-worker (worker)
  (cond ((symbolp worker) worker)
        ((integerp worker) worker)
        ((typep worker 'worker) worker)
        (t (error "Invalid local worker: ~S" worker))))

(defmethod initialize-instance :before ((recipient local-recipient) &rest initargs &key worker &allow-other-keys)
  (ensure-valid-local-worker worker))

(defun local-recipient (&key worker)
  (make-instance 'local-recipient :worker worker))

;;; recipients for workers in a different Lisp process or on a different host
(defclass remote-recipient ()
  ((host :reader host :initform nil :initarg :host)
   (port :reader port :initform nil :initarg :port)
   (worker :reader worker :initform nil :initarg :worker)))

(defun ensure-valid-remote-host (host)
  (cond ((stringp host) host)
        (t (error "Invalid remote host: ~S" host))))

(defun ensure-valid-remote-port (port)
  (cond ((integerp port) port)
        (t (error "Invalid remote port: ~S" port))))

(defun ensure-valid-remote-worker (worker)
  (cond ((symbolp worker) worker)
        ((integerp worker) worker)
        (t (error "Invalid remote worker: ~S" worker))))

(defmethod initialize-instance :before ((recipient remote-recipient) &rest initargs
                                        &key host port worker &allow-other-keys)
  (ensure-valid-remote-host host)
  (ensure-valid-remote-port port)
  (ensure-valid-remote-worker worker))

(defun remote-recipient (&key host port worker)
  (make-instance 'remote-recipient :host host :port port :worker worker))

#+nil (setf $local (local-recipient))
#+nil (describe $local)
#+nil (setf $remote (make-instance 'remote-recipient))
#+nil (setf $remote (make-instance 'remote-recipient :host "192.168.0.159" :port *message-receive-port*))
#+nil (describe $remote)
