;;;; ***********************************************************************
;;;;
;;;; Name:          build.asd
;;;; Project:       Apis: the hive application
;;;; Purpose:       build file for ltk on Linux
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ---------------------------------------------------------------------
;;; apis system
;;; ---------------------------------------------------------------------
;;;; apis.asd

(asdf:defsystem :apis-ltk
  :name "apis"
  :version "0.1.0"
  :author "mikel evins"
  :serial t
  :depends-on (:apis)
  :components ((:file "package")
               (:file "config")
               (:file "ltk")
               (:file "main")
               (:file "build")))

;;; (asdf:load-system :apis-ltk)

(defun load-ltk ()
  (asdf::oos 'asdf:compile-op :apis-ltk)
  (asdf::oos 'asdf:load-op :apis-ltk))

(defun build-ltk ()
  (load-ltk)
  (build-bundle)
  (build-image))

;;; (build-ltk)
