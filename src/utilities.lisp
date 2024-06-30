;;;; ***********************************************************************
;;;;
;;;; Name:          utilities.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       useful general-purpose functions 
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; sequences
;;; ---------------------------------------------------------------------

(defmethod cat ((s1 sequence)(s2 sequence))
  (concatenate (class-of s1) s1 s2))

#+nil (cat '(1 2 3) '(4 5 6))
#+nil (cat "123" "456")
#+nil (cat '(1 2 3) "456")
#+nil (cat "123" '(#\4 #\5))
