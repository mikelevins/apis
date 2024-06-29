;;;; ***********************************************************************
;;;;
;;;; Name:          singleton-class.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       classes that have only one instance
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defclass singleton-class (cl:standard-class)
  ((instance :accessor instance :initform nil)))

;;; a standard-class may be a superclass of a singleton-class
(defmethod closer-mop:validate-superclass ((class singleton-class)(superclass cl:standard-class))
  t)

;;; a singleton-class may be a superclass of a singleton-class
(defmethod closer-mop:validate-superclass ((class singleton-class)(superclass singleton-class))
  t)

;;; a singleton-class may not be a superclass of a non-singleton-class 
(defmethod closer-mop:validate-superclass ((class cl:standard-class)(superclass singleton-class))
  nil)

(defmethod make-instance ((class singleton-class) &rest args &key)
  (unless (instance class)
    (setf (instance class)(call-next-method)))
  (instance class))

(defmethod reset-singleton-class ((class singleton-class))
  (setf (instance class)
        nil))
