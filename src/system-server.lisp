;;;; ***********************************************************************
;;;;
;;;; Name:          apis.lisp
;;;; Project:       apis: worker bees for application hives
;;;; Purpose:       main program
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defroute (:get "/")(request response)
  (send-response response :body "Welcome to apis"))

;;; TODO: write config support so we can find
;;;       this directory some real way
(def-directory-route "/" (server-path "public/"))

(defparameter *server* nil)

(defun start ()
  (unless *server*
    (as:with-event-loop ()
      ;; create a listener object, and pass it to start-server, which starts Wookie
      (let* ((listener (make-instance 'listener
                                      :bind nil  ; equivalent to "0.0.0.0" aka "don't care"
                                      :port 4242)))
        (setf *server* (start-server listener))
        *server*))))

(defun stop ()
  (when *server*
    (let ((server *server*))
      (as:close-tcp-server server)
      (setf *server* nil)
      server)))

;;; (start)
;;; (stop)

