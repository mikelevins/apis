;;;; ***********************************************************************
;;;;
;;;; Name:          build.lisp
;;;; Project:       Apis: the hive application
;;;; Purpose:       build the application bundle
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ---------------------------------------------------------------------
;;; locating project assets
;;; ---------------------------------------------------------------------

(defun image-path ()
  (merge-pathnames *app-name* *project-root*))

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(defun build-image ()
  (save-application (image-path) :application-class 'apis::apis-application :prepend-kernel t))
