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

(defparameter *server* (make-instance 'hunchentoot:easy-acceptor :port 10101))

#| example route
(hunchentoot:define-easy-handler (root :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (format nil "<html><body><p>apis</p></body></html>"))
|#

;;; (hunchentoot:start *server*)
;;; (hunchentoot:stop *server*)
