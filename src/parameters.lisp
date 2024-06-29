;;;; ***********************************************************************
;;;;
;;;; Name:          parameters.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       constants and variables that paramterize agent behavior
;;;; Author:        mikel evins
;;;; Copyright:     2023 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.apis)

(defparameter *uuid-random-state* (make-random-state t))
(defparameter *message-receive-port* 10764)
(defparameter *maximum-messenger-buffer-size* 32767)
(defparameter *default-message-time-to-live* 600) ; seconds
