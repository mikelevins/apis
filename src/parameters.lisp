;;;; ***********************************************************************
;;;;
;;;; Name:          parameters.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       system parameters
;;;; Author:        mikel evins
;;;; Copyright:     2015-2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defparameter *id-random-state* (make-random-state t))
(defparameter *default-message-time-to-live* 600) ; seconds
(defparameter *default-runtime-thread-count* 4)
(defparameter *default-runtime* nil)
(defparameter *dead-letters* (make-array 16 :initial-element nil :fill-pointer 0 :adjustable t))
