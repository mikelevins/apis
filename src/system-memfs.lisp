;;;; ***********************************************************************
;;;;
;;;; Name:          system-memfs.lisp
;;;; Project:       apis: worker bees for application hives
;;;; Purpose:       in-memory filesystem for modeling projects
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; memfs is a simple in-memory folesystem that apis uses to model
;;; project resources. the idea is that apis contains a memfs in its
;;; memory image that in turn contains images of resource files needed
;;; for application projects. when apis creates a new project it
;;; constructs a working project directory by copying resource files
;;; from its memfs to the project directory. similarly, the
;;; installer created when apis builds an application can copy
;;; application resources to a destination directory when it runs
;;; the installation.
;;; memfs is the code that models an in-memory filesystem for these
;;; purposes.

