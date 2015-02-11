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

;;; ignore the Finder's -psn argument
(defmethod ccl::parse-application-arguments ((a apis-application))
  (values nil nil nil nil))

;;; ------------------------------------------------------------
;;; the Apis main function
;;; the toplevel-function runs the Cocoa NSApp object

(defmethod toplevel-function ((app apis-application) init-file)
  (declare (ignore init-file))
  (ccl::with-autorelease-pool
    (let* ((app (ccl::nsapp))
           (delegate (#/autorelease (#/init (#/alloc apis-app-delegate)))))
      (#/setDelegate: app delegate)
      (setup-menus)
      (ccl::run-event-loop))))
