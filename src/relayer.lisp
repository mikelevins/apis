;;;; ***********************************************************************
;;;;
;;;; Name:          relayer.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       a worker the exchanged remote messages
;;;; Author:        mikel evins
;;;; Copyright:     2015-2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; CLASS relayer
;;; ---------------------------------------------------------------------

(defclass relayer (worker)
  ()
  (:metaclass singleton-class))

(defun the-relayer () (make-instance 'relayer))
