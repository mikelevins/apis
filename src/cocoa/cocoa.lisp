;;;; ***********************************************************************
;;;;
;;;; Name:          cocoa.lisp
;;;; Project:       Apis: the hive application
;;;; Purpose:       cocoa support
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :ccl)

;;; ------------------------------------------------------------
;;; load Cocoa support

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-interface-dir :cocoa)
  (require "OBJC-SUPPORT"))

;;; ------------------------------------------------------------
;;; logging utility

(defun nslog (c)
  (let* ((rep (format nil "~a" c)))
    (ccl::with-cstrs ((str rep))
      (ccl::with-nsstr (nsstr str (length rep))
        (#_NSLog #@"Logging: %@" :address nsstr)))))

;;; ------------------------------------------------------------
;;; setup Cocoa app object to run

;;; get the shared application
(defun nsapp ()
  (#/sharedApplication ns:ns-application))

;;; ------------------------------------------------------------
;;; run the Cocoa app's event loop

(defun run-event-loop ()
  (%set-toplevel nil)
  (let* ((app (nsapp)))
    (loop
       (handler-case (#/run app) (error (c) (nslog c)))
       (unless (#/isRunning app) (return)))))

(in-package :apis)

;;; ---------------------------------------------------------------------
;;; objc utils
;;; ---------------------------------------------------------------------

(defun perform-selector (obj selector &key (with-object nil))
  (if with-object
      (#/performSelector:withObject: obj selector with-object)
      (#/performSelector: obj selector)))



