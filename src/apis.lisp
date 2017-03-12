;;;; ***********************************************************************
;;;;
;;;; Name:          apis.lisp
;;;; Project:       apis: a worker bee for application hives
;;;; Purpose:       main program
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defparameter *server* (make-instance 'hunchentoot:easy-acceptor :port 10101))

(hunchentoot:define-easy-handler (root :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (format nil "<html><body><p>apis</p></body></html>"))

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/html")
  (format nil "<html><body><p>Hello, ~A.</p></body></html>" name))

;;; (hunchentoot:start *server*)
;;; (hunchentoot:stop *server*)
