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

(defparameter *repl-thread* nil)

(defun stage-message-handler (application message &rest args)
  (case message
    (t nil)))

(defparameter *stage-windows* (make-hash-table))

(defun stage-create-window (application args)
  (let* ((x (getf args :x 100))
         (y (getf args :y 100))
         (width (getf args :width 800))
         (height (getf args :height 600))
         (id (apis::makeid))
         (win (make-instance 'interface :best-x x :best-y y
                             :best-width width :best-height height)))
    (setf (gethash id *stage-windows*) win)
    (display win)
    `(:window ,id)))

(defun handle-repl-message (application message)
  (assert (listp message)() "Malformed message on repl thread: ~S" message)
  (let ((msg-type (car message))
        (msg-args (cdr message)))
    (case msg-type
      ((:quit) (throw :stage-exit :quit))
      ((:set-title) (first msg-args))
      ((:create-window) (stage-create-window application msg-args))
      (t (warn "Unrecognized message type ~S on repl thread" msg-type)))))

(defun stage-repl (application)
  (catch :stage-exit
    (loop
       (let* ((message (read *standard-input* nil nil nil))
              (response (handle-repl-message application message)))
         (format *standard-output* "~S~%" response))))
  (capi:apply-in-pane-process application
                              (capi:quit-interface application)))

(defun run-stage-application ()
  (let ((application (make-instance '<stage-application>)))
    (setf *repl-thread*
          (bt:make-thread (lambda ()(stage-repl application))
                          :name "stage-repl-process"))
    ;; Set the application interface before using any other CAPI
    ;; functionality.
    (capi:set-application-interface application)
    ;; Start the application with no windows initially.
    (capi:convert-to-screen nil)))
