;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          absent.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       a singleton class that represents an absence of any useful value
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; absent
;;; ---------------------------------------------------------------------
;;; represents the absence of any useful value

(defclass absent ()()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defmethod print-object ((a absent)(out stream))
  (print-unreadable-object (a out :type t :identity nil)
    ))

(defmethod absent? (x)
  (declare (ignore x))
  nil)

(defmethod absent? ((x absent))
  (declare (ignore x))
  t)

(defun present? (x)
  (not (absent? x)))

(defun absent ()
  (make-instance 'absent))
