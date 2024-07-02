;;;; ***********************************************************************
;;;;
;;;; Name:          parameters.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       constants and variables that paramterize worker behavior
;;;; Author:        mikel evins
;;;; Copyright:     2023 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defparameter +prng+
  (let ((prng (ironclad:make-prng :fortuna)))
    (crypto:read-os-random-seed :random prng)
    prng))

(defparameter *session-id-counter* 0)
(defparameter +max-session-id-counter+ #b1111111111111111)

(defparameter *localhost* "127.0.0.1")
(defparameter *message-receive-port* 10764)
(defparameter *maximum-buffer-size* 32767)
