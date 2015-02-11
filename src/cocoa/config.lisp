;;;; ***********************************************************************
;;;;
;;;; Name:          config-template.lisp
;;;; Project:       Apis: the hive application
;;;; Purpose:       build-time parameters
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; edit the parameters in this file to create config.lisp, which Apis
;;; will use for build-time configuration

(in-package :cl-user)

;;; ---------------------------------------------------------------------
;;; configuration parameters
;;; ---------------------------------------------------------------------
;;; Edit these:

(defparameter *project-root*
  (pathname "/Users/mikel/Workshop/programming/apis/src/cocoa/"))

(defparameter *lisp-directory*
  (pathname "/usr/local/Cellar/clozure-cl/1.10/libexec/"))

(defparameter *lisp*
  (merge-pathnames "dx86cl64" *lisp-directory*))

(defparameter *app-name* "Apis")

(defparameter *bundle-name* (concatenate 'string *app-name* ".app"))

