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
;;; dev-time path utils
;;; ---------------------------------------------------------------------

(let* ((path *load-truename*)
       (project-root (make-pathname :directory (pathname-directory path))))
  ;;; when the app is delivered, we redefine path-base to resolve
  ;;; paths relative to the app bundle
  (defun path-base () project-root))

(defun path (p)(merge-pathnames p (path-base)))

(defun add-to-asdf (path)
  (pushnew (truename (merge-pathnames path (path-base)))
           asdf:*central-registry* :test 'equalp))

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
               (:file "cocoa")
               (:file "delegate")
               (:file "menus")
               (:file "main-menu")
               (:file "main")
               (:file "make")))
;;; (asdf:load-system :apis)

(defun load-apis ()
  (asdf::oos 'asdf:compile-op :apis)
  (asdf::oos 'asdf:load-op :apis))

(defun build-apis (path)
  (load-apis)
  (build-image path))

