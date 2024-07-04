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

;;; Apis epoch 
;;; CommitDate: Wed Apr 2 18:46:47 2014 -0500
;;; (local-time:universal-to-timestamp 3605471207)
;;; => @2014-04-02T18:46:47.000000-05:00
(defparameter +apis-epoch+ 3605471207)

(defparameter +prng+
  (let ((prng (ironclad:make-prng :fortuna)))
    (crypto:read-os-random-seed :random prng)
    prng))

(defparameter *session-id-counter* 0)
(defparameter +max-session-id-counter+ #b1111111111111111)

(defparameter *localhost* "127.0.0.1")
(defparameter *message-receive-port* 10764)
(defparameter *maximum-buffer-size* 32767)
(defparameter *default-message-time-to-live* 600) ; seconds
