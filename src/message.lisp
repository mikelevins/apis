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
  ((id :reader message-id :initform (makeid) :initarg :id :type integer)
   ;; a ULID identifying the sender, or nil
   (from :reader message-from :initform nil :initarg :from :type (or integer null))
   ;; a ULID identifying the recipient
   (to :reader message-to :initform nil :initarg :to :type (or integer null))
   ;; a keyword naming the operation
   (operation :reader message-operation :initform :ping :initarg :operation :type symbol)
   ;; a plist
   (data :reader message-data :initform nil :initarg :data)
   (timestamp :reader message-timestamp :initform (get-universal-time) :initarg :timestamp :type integer)
   (time-to-live :reader message-time-to-live
                 :initform *default-message-time-to-live* :initarg :time-to-live :type integer)
   ;; a ULID identifying the message that caused this one, or nil
   (cause :reader message-cause :initform nil :initarg :cause :type (or integer null))))

(defmethod print-object ((obj message) out-stream)
  (print-unreadable-object (obj out-stream :type nil :identity nil)
    (format out-stream "message ~A from: ~A to: ~A ~S ~S"
            (format-id (message-id obj))
            (if (message-from obj) (format-id (message-from obj)) "nil")
            (if (message-to obj) (format-id (message-to obj)) "nil")
            (message-operation obj)
            (message-data obj))))

(defun message (&key (id (makeid)) from to (operation :ping) data
                  (timestamp (get-universal-time))
                  (time-to-live *default-message-time-to-live*)
                  cause)
  (make-instance 'message :id id :from from :to to :operation operation :data data
                          :timestamp timestamp :time-to-live time-to-live
                          :cause cause))

#+repl (defvar *msg* (apis:message :from *id1* :to *id2* :operation :greet :data '(:name "Alice")))
#+repl (describe *msg*)
#+repl (defvar *reply* (apis:message :to *id1* :operation :ack :cause (apis:message-id *msg*)))
#+repl (describe *reply*)
