;;;; ***********************************************************************
;;;;
;;;; Name:          system-project.lisp
;;;; Project:       apis: worker bees for application hives
;;;; Purpose:       constructing an Apis project
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; build a project
;;; ---------------------------------------------------------------------

;;; validate-root-path
;;; ---------------------------------------------------------------------
;;; check a proposed project pathname to make sure its parent directory
;;; exists or can be created; if the project root can be created, return
;;; its absolute pathname as a directory pathname

(defmethod validate-root-path ((path pathname))
  (let ((existing-path (probe-file
                        (uiop/filesystem::ensure-pathname path
                                                          :ensure-directory t
                                                          :ensure-directories-exist nil))))
    (if existing-path
        ;; TODO: make sure it doesn't already have stuff in it
        existing-path
        ;; TODO: figure out whether we can create it
        nil)))

(defmethod validate-root-path ((path string))
  (validate-root-path (pathname path)))

;;; (validate-root-path "/Users/mikel")
;;; (validate-root-path "/Users/grobnar")
