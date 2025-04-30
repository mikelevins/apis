;;;; ***********************************************************************
;;;;
;;;; Name:          id.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       simple compact ids
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defun makeid (&key time random-integer)
  (+ (ash (or time (get-universal-time)) 32)
     (or random-integer (random #xFFFFFFFF *id-random-state*))))
