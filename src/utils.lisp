;;;; ***********************************************************************
;;;;
;;;; Name:          utils.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       common utility functions and parameters
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defmethod low-n-bits ((n integer)(int integer))
  (let ((len (integer-length int)))
    (if (<= len n)
        int
        (ldb (byte n 0)
             int))))

#+nil (low-n-bits 2 #b1011)
#+nil (low-n-bits 4 #b110101011)
#+nil (integer-length #b110101011)
#+nil (ldb (byte 2 0) #b100101010)
