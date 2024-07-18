;;;; ***********************************************************************
;;;;
;;;; Name:          dispatcher.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       local delivery of remote messages
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defclass dispatcher (worker)
  ()
  (:metaclass singleton-class))

(defun the-dispatcher ()
  (make-instance 'dispatcher))

(defun reset-the-dispatcher ()
  (reset-singleton-class (find-class 'dispatcher)))

#+nil (eq (the-dispatcher)(the-dispatcher))
#+nil (reset-the-dispatcher)
