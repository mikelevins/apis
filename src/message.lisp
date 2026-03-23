;;;; ***********************************************************************
;;;;
;;;; Name:          message.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       message implementation
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; CLASS message
;;; ---------------------------------------------------------------------

(defclass message ()
  ((id :reader message-id :initform (makeid) :initarg :id :type integer)
   ;; address: a ULID (integer, local) or URI string (remote), or nil
   (from :reader message-from :initform nil :initarg :from :type (or integer string null))
   ;; address: a ULID (integer, local) or URI string (remote), or nil
   (to :reader message-to :initform nil :initarg :to :type (or integer string null))
   ;; a keyword naming the operation
   (operation :reader message-operation :initform :ping :initarg :operation :type symbol)
   ;; a plist
   (data :reader message-data :initform nil :initarg :data)
   (timestamp :reader message-timestamp :initform (get-universal-time) :initarg :timestamp :type integer)
   (time-to-live :reader message-time-to-live
                 :initform *default-message-time-to-live* :initarg :time-to-live :type integer)
   ;; a ULID identifying the message that caused this one, or nil
   (cause :reader message-cause :initform nil :initarg :cause :type (or integer null))))

(defun print-address (address)
  "Format an address for human-readable display."
  (etypecase address
    (null "nil")
    (integer (format-id address))
    (string address)))

(defmethod print-object ((obj message) out-stream)
  (print-unreadable-object (obj out-stream :type nil :identity nil)
    (format out-stream "message ~A from: ~A to: ~A ~S ~S"
            (format-id (message-id obj))
            (print-address (message-from obj))
            (print-address (message-to obj))
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
