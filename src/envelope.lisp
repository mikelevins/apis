;;;; ***********************************************************************
;;;;
;;;; Name:          envelope.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       delivery information for messages
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defclass envelope ()
  ((from-host :accessor from-host :initform nil :initarg :from-host)
   (from-port :accessor from-port :initform nil :initarg :from-port)
   (from-worker :accessor from-worker :initform nil :initarg :from-worker)
   (to-host :accessor to-host :initform nil :initarg :to-host)
   (to-port :accessor to-port :initform nil :initarg :to-port)
   (to-worker :accessor to-worker :initform nil :initarg :to-worker)
   (message :accessor message :initform nil :initarg :message)))

(defmethod print-object ((obj envelope) out-stream)
  (print-unreadable-object (obj out-stream :type t :identity nil)
    (let* ((dest-worker (to-worker obj))
           (dest-id (cond ((workerp dest-worker)(format nil "~X" (worker-id dest-worker)))
                          ((integerp dest-worker) (format nil "~X" dest-worker))
                          ((null dest-worker) "apis"))))
      (format out-stream "from: ~A@~A:~A to: ~A@~A:~A"
              (or (from-worker obj) "apis")
              (or (from-host obj) "localhost")
              (or (from-port obj) *message-receive-port*)
              dest-id
              (or (to-host obj) "localhost")
              (or (to-port obj) *message-receive-port*)))))

