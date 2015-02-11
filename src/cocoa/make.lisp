;;;; ***********************************************************************
;;;;
;;;; Name:          make.lisp
;;;; Project:       Apis: the hive application
;;;; Purpose:       Build the app's executable image
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:cl-user)

(defun build-image (path)
  (save-application path :application-class 'apis::apis-application :prepend-kernel t))

