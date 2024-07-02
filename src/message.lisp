;;;; ***********************************************************************
;;;;
;;;; Name:          message.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       representation of messages
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; message
;;; ---------------------------------------------------------------------
;;; the data transmitted from worker to worker

(defparameter *default-message-time-to-live* 600) ; seconds

(defclass message ()
  ((operation :reader message-operation :initform :ping :initarg :operation)
   (data :reader message-data :initform nil :initarg :data)
   (timestamp :reader message-timestamp :initform (get-universal-time) :initarg :timestamp)
   (time-to-live :reader message-time-to-live :initform *default-message-time-to-live* :initarg :time-to-live)))

(defmethod print-object ((obj message) out-stream)
  (print-unreadable-object (obj out-stream :type t :identity nil)
    (let ((timestamp (message-timestamp obj)))
      (format out-stream "~S ~S"
              (message-operation obj)
              (message-data obj)))))

#+nil (defparameter $msg1 (make-instance 'message))
#+nil (describe $msg1)

