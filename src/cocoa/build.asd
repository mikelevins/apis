;;;; ***********************************************************************
;;;;
;;;; Name:          apis.asd
;;;; Project:       Apis: the hive application
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require "OBJC-SUPPORT")
(require :asdf)

;;; ---------------------------------------------------------------------
;;; apis system
;;; ---------------------------------------------------------------------
;;;; apis.asd

(asdf:defsystem :apis
  :name "apis"
  :version "0.1.0"
  :author "mikel evins"
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "cocoa")
               (:file "delegate")               
               (:file "menus")
               (:file "main-menu")
               (:file "main")
               (:file "build")))
;;; (asdf:load-system :apis)

(defun load-apis ()
  (asdf::oos 'asdf:compile-op :apis)
  (asdf::oos 'asdf:load-op :apis))

(defun build-apis ()
  (load-apis)
  (build-bundle)
  (build-image))

;;; (build-apis)
