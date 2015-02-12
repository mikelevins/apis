;;;; ***********************************************************************
;;;;
;;;; Name:          stage.lisp
;;;; Project:       the stage presentation server for apis
;;;; Purpose:       main program
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :stage)

#+:cocoa
(define-interface <stage-application> (capi:cocoa-default-application-interface)
  ()
  (:menus)
  (:menu-bar)
  (:default-initargs
      :title "Stage"
    :message-callback 'stage-message-handler))

(defun stage-message-handler (application message &rest args)
  (case message
    (t nil)))

(defun run-stage-application ()
  (let ((application (make-instance '<stage-application>)))
    ;; Set the application interface before using any other CAPI
    ;; functionality.
    (capi:set-application-interface application)
    ;; Start the application with no windows initially.
    (capi:convert-to-screen nil)))
