;;;; ***********************************************************************
;;;;
;;;; Name:          main.lisp
;;;; Project:       Apis: the hive application
;;;; Purpose:       build the Apis application image
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :apis)

;;; ------------------------------------------------------------
;;; the Apis application class

(defclass apis-application (ccl::application) ())

;;; ------------------------------------------------------------
;;; the Apis main function
;;; the toplevel-function runs the Cocoa NSApp object

(defmethod toplevel-function ((app apis-application) init-file)
  (declare (ignore init-file))
  ;; TODO: write ltk toplevel
  )
