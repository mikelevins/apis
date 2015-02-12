;;;; ***********************************************************************
;;;;
;;;; Name:          comm-message.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       messages represented as entities
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; message
;;; ---------------------------------------------------------------------

(defmethod message? (x) nil)

(defmethod message? ((x cons))
  (list (eq :message (car x))
        (listp (cdr x))))

(deftype message ()
  `(and list (satisfies message?)))

(defun message (&rest properties &key
                                   (timestamp (get-universal-time))
                                   (id (makeid))
                                   &allow-other-keys)
  (merge-entities (cons :message properties)
                  `(:message :id ,id :timestamp ,timestamp)))

;;; (message :subject "greeting")
;;; (message :subject "greeting" :id 1)

