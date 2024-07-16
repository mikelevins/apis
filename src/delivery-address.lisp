;;;; ***********************************************************************
;;;;
;;;; Name:          delivery-address.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       delivery information for messages
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defclass delivery-address ()
  ((host :accessor host :initform nil :initarg :host)
   (port :accessor port :initform nil :initarg :port)
   (worker :accessor worker :initform nil :initarg :worker)))

(defmethod print-object ((obj delivery-address) out-stream)
  (print-unreadable-object (obj out-stream :type t :identity nil)
    (let* ((worker (worker obj))
           (id (cond ((workerp worker)(format nil "~X" (worker-id-string worker)))
                     ((stringp worker) (format nil "~A" worker))
                     ((integerp worker) (format nil "~X" worker))
                     ((null worker) "apis"))))
      (format out-stream "~A@~A:~A"
              id
              (or (host obj) "localhost")
              (or (port obj) *message-receive-port*)))))

(defun delivery-address (&key (host "localhost")(port *message-receive-port*)(worker nil))
  (make-instance 'delivery-address
                 :host host
                 :port port
                 :worker worker))

#+nil (delivery-address)
