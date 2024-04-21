;;;; ***********************************************************************
;;;;
;;;; Name:          parameters.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       constants and variables that paramterize agent behavior
;;;; Author:        mikel evins
;;;; Copyright:     2023 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(setf cl:*random-state* (make-random-state t))

(defparameter *session-id-counter* 0)
(defparameter +max-session-id-counter+ #b1111111111111111)
