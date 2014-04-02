;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          identifier.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       unique ids for agents and messages
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; this version of makeid generates a v4 uuid and converts it to 
;;; a 128-bit integer
(defun makeid ()
  (let* ((uuid (uuid:make-v4-uuid))
         (bytes (uuid:uuid-to-byte-array uuid))
         (max-index (1- (length bytes))))
    (loop for byte across bytes
      for i from 0
      summing (ash byte (* 8 (- max-index i))) into total
      finally (return total))))