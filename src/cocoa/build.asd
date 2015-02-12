;;;; ***********************************************************************
;;;;
;;;; Name:          build.asd
;;;; Project:       Apis: the hive application
;;;; Purpose:       build file for Cocoa on OSX
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require "OBJC-SUPPORT")
(require :asdf)

;;; ---------------------------------------------------------------------
;;; apis system
;;; ---------------------------------------------------------------------
;;;; apis.asd

(asdf:defsystem :apis-cocoa
  :name "apis-cocoa"
  :version "0.1.0"
  :author "mikel evins"
  :serial t
  :depends-on (:apis)
  :components ((:file "package")
               (:file "config")
               (:file "cocoa")
               (:file "delegate")               
               (:file "menus")
               (:file "main")
               (:file "build")))

;;; (asdf:load-system :apis-cocoa)

(defun load-cocoa ()
  (asdf::oos 'asdf:compile-op :apis-cocoa)
  (asdf::oos 'asdf:load-op :apis-cocoa))

(defun build-cocoa ()
  (load-cocoa)
  (build-bundle)
  (build-image))

;;; (build-cocoa)
