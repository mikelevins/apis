;;;; ***********************************************************************
;;;;
;;;; Name:          config.lisp
;;;; Project:       Apis: the hive application
;;;; Purpose:       build-time parameters
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ---------------------------------------------------------------------
;;; configuration parameters
;;; ---------------------------------------------------------------------
;;; Edit these:

(defparameter *project-root*
  (pathname "/home/mikel/Workshop/apis/src/ltk/"))

(defparameter *lisp-directory*
  (pathname "/usr/share/ccl"))

(defparameter *lisp* "/usr/bin/ccl64")

(defparameter *app-name* "Apis")


