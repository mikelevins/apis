;;;; ***********************************************************************
;;;;
;;;; Name:          build.asd
;;;; Project:       Apis: the hive application
;;;; Purpose:       build file for windows
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ---------------------------------------------------------------------
;;; apis system
;;; ---------------------------------------------------------------------
;;;; apis.asd

(asdf:defsystem :apis-windows
  :name "apis"
  :version "0.1.0"
  :author "mikel evins"
  :serial t
  :depends-on (:apis)
  :components ((:file "package")
               (:file "config")
               (:file "windows")
               (:file "main")
               (:file "build")))

;;; (asdf:load-system :apis-windows)

(defun load-windows ()
  (asdf::oos 'asdf:compile-op :apis-windows)
  (asdf::oos 'asdf:load-op :apis-windows))

(defun build-windows ()
  (load-windows)
  (build-image))

;;; (build-windows)

