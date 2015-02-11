;;;; ***********************************************************************
;;;;
;;;; Name:          config-template.lisp
;;;; Project:       Apis: the hive application
;;;; Purpose:       build-time parameters
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
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
  (pathname "c:/Users/mevin_000/Workshop/apis/src/windows/"))

(defparameter *lisp-directory*
  (pathname "C:/ccl/"))

(defparameter *lisp* "C:/ccl/wx86cl64.exe")

(defparameter *app-name* "Apis")


