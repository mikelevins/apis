;;;; ***********************************************************************
;;;;
;;;; Name:          logging.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       simple logging facility
;;;; Author:        mikel evins
;;;; Copyright:     2023 by mikel evins
;;;;
;;;; ***********************************************************************


(defpackage #:log
  (:use #:cl)
  (:shadow #:debug)
  (:export
   #:*log-level*
   #:+log-levels+
   #:debug
   #:info))

(in-package :log)

(defparameter +log-levels+ '(:none :info :debug))

;;; if *log-level* is:
;;;  :none - no logging messages are printed
;;;  :info - info messages are printed
;;;  :debug - info and debug messages are printed

(defparameter *log-level* :info)

(defun info (stream format-string &rest args)
  (when (member *log-level* '(:info :debug) :test 'eql)
    (apply 'format stream format-string args)))

(defun debug (stream format-string &rest args)
  (when (eql :debug *log-level*)
    (apply 'format stream format-string args)))
