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

(defparameter *dead-messages* (make-array 32 :initial-element nil :fill-pointer 0 :adjustable t))
(defmethod file-dead-message ((message message) &key (explanation "Message delivery failed."))
  (let ((dm (cons explanation message)))
    (format t "~A: ~S" explanation message)
    (vector-push-extend dm *dead-messages* 16)))

;;; ---------------------------------------------------------------------
;;; CLASS message
;;; ---------------------------------------------------------------------

(defclass message ()
  (;; a vector of 20 bytes
   (id :reader message-id :initform (makeid) :initarg :id :type 'integer)
   ;; a delivery-address
   (from :reader message-from :initform nil :initarg :from)
   ;; a delivery-address
   (to :reader message-to :initform nil :initarg :to)
   ;; a keyword
   (operation :reader message-operation :initform :ping :initarg :operation :type (or null symbol))
   (arguments :reader message-arguments :initform nil :initarg :arguments :type (or null list))
   (timestamp :reader message-timestamp :initform (get-universal-time) :initarg :timestamp :type 'integer)
   (time-to-live :reader message-time-to-live
                 :initform *default-message-time-to-live* :initarg :time-to-live :type 'integer)))

(defmethod print-object ((obj message) out-stream)
  (print-unreadable-object (obj out-stream :type t :identity nil)
    (format out-stream "~X ~S"
            (message-id obj)
            (message-operation obj))))

(defun message (&key (id (makeid)) from to (operation :ping) arguments
                  (timestamp (get-universal-time))(time-to-live *default-message-time-to-live*))
  (make-instance 'message :id id :from from :to to :operation operation :arguments arguments
                          :timestamp timestamp :time-to-live time-to-live))

(defmethod send ((msg message))
  (let ((to (message-to msg)))
    (if to
        (let ((q (worker-message-queue to)))
          (bt:with-recursive-lock-held ((queues::lock-of q))
            (queues:qpush q msg)
            (bt:signal-semaphore (worker-message-semaphore to))))
        (file-dead-message msg))))
