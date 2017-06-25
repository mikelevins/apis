;;;; ***********************************************************************
;;;;
;;;; Name:          system-paths.lisp
;;;; Project:       apis: worker bees for application hives
;;;; Purpose:       utilities for finding apis system pathnames
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defun server-path (path-fragment)
  (merge-pathnames path-fragment
                   *server-root-path*))
