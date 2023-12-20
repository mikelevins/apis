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
   #:disable-logging
   #:enable-logging
   #:info
   #:logging-enabled?))

(in-package :log)

(defparameter +log-levels+ '(:none :info :debug))

;;; if *log-level* is:
;;;  :none - no logging messages are printed
;;;  :info - info messages are printed
;;;  :debug - info and debug messages are printed

(defparameter *logging-enabled* nil)
(defparameter *log-level* :info)

(defun enable-logging ()
  (setf *logging-enabled* t))

(defun disable-logging ()
  (setf *logging-enabled* nil))

(defun logging-enabled? ()
  *logging-enabled*)

(defun info (stream format-string &rest args)
  (when (and *logging-enabled*
             (member *log-level* '(:info :debug) :test 'eql))
    (apply 'format stream format-string args)))

(defun debug (stream format-string &rest args)
  (when (and *logging-enabled*
             (eql :debug *log-level*))
    (apply 'format stream format-string args)))
