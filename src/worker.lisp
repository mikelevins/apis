;;;; ***********************************************************************
;;;;
;;;; Name:          worker.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       worker implementation
;;;; Author:        mikel evins
;;;; Copyright:     2015-2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; CLASS worker
;;; ---------------------------------------------------------------------

(defclass worker ()
  ((id :reader worker-id :initform (makeid) :initarg :id :type integer)
   (description :reader worker-description :initform nil :initarg :description :type (or string null))
   (message-queue :accessor worker-message-queue :initform (make-instance 'queues:simple-cqueue))
   (state :accessor worker-state :initform :idle :type keyword)))

(defmethod print-object ((worker worker) out-stream)
  (print-unreadable-object (worker out-stream :type t :identity nil)
    (format out-stream "~A [~(~A~)]"
            (format-id (worker-id worker))
            (worker-state worker))))

;;; ---------------------------------------------------------------------
;;; auto-registration in the default runtime
;;; ---------------------------------------------------------------------

(defmethod initialize-instance :after ((worker worker) &key)
  (when *default-runtime*
    (let ((registry (slot-value *default-runtime* 'registry))
          (lock (slot-value *default-runtime* 'registry-lock)))
      (bt:with-lock-held (lock)
        (setf (gethash (worker-id worker) registry) worker)))))

;;; ---------------------------------------------------------------------
;;; handle-message protocol
;;; ---------------------------------------------------------------------

(define-condition unhandled-message (error)
  ((message :initarg :message :reader unhandled-message)))

(defgeneric handle-message (worker message operation data))

(defmethod handle-message ((worker worker) (msg message) op data)
  "Default method: signal an UNHANDLED-MESSAGE error."
  (error 'unhandled-message :message msg))

(defmethod handle-message ((worker worker) (msg message) (op (eql :ping)) data)
  (format t "~&~S received a :PING message (~S)~%" worker msg))

(defmethod receive ((worker worker) (msg message))
  (let ((op (message-operation msg))
        (data (message-data msg)))
    (handler-case (handle-message worker msg op data)
      (unhandled-message (err)
        (warn "received ~S message not handled (~S)" op msg)
        nil))))

#+repl (defvar *w1* (make-instance 'apis:worker))
#+repl (defvar *msg* (apis:message :to (apis:worker-id *w1*) :operation :ping))
#+repl (apis:receive *w1* *msg*)
