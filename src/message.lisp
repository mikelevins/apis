;;;; ***********************************************************************
;;;;
;;;; Name:          message.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       message implementation
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; CLASS message
;;; ---------------------------------------------------------------------

(defclass message ()
  (;; a vector of 20 bytes
   (id :reader message-id :initform (makeid) :initarg :id :type integer)
   ;; a worker
   (from :reader message-from :initform nil :initarg :from)
   ;; a worker
   (to :reader message-to :initform nil :initarg :to)
   ;; a keyword
   (operation :reader message-operation :initform :ping :initarg :operation :type (or null symbol))
   ;; a plist
   (data :reader message-data :initform nil :initarg :data)
   (timestamp :reader message-timestamp :initform (get-universal-time) :initarg :timestamp :type integer)
   (time-to-live :reader message-time-to-live
                 :initform *default-message-time-to-live* :initarg :time-to-live :type integer)))

(defmethod print-object ((obj message) out-stream)
  (print-unreadable-object (obj out-stream :type nil :identity nil)
    (format out-stream "message ~X from: ~S to: ~S ~S ~S"
            (type-of obj)
            (type-of (message-from obj))
            (type-of (message-to obj))
            (message-operation obj)
            (message-data obj))))

(defun message (&key (id (makeid)) from to (operation :ping) data
                  (timestamp (get-universal-time))(time-to-live *default-message-time-to-live*))
  (make-instance 'message :id id :from from :to to :operation operation :data data
                          :timestamp timestamp :time-to-live time-to-live))


(defparameter *dead-messages* (make-array 32 :initial-element nil :fill-pointer 0 :adjustable t))
(defmethod file-dead-message ((message message) &key (explanation "Message delivery failed."))
  (let ((dm (cons explanation message)))
    (format t "~A: ~S" explanation message)
    (vector-push-extend dm *dead-messages* 16)))

(defmethod send ((msg message))
  (let ((to (message-to msg)))
    (if to
        (let ((q (message-queue to)))
          (bt:with-recursive-lock-held ((queues::lock-of q))
            (queues:qpush q msg)
            (bt:signal-semaphore (message-semaphore to))))
        (file-dead-message msg))))
