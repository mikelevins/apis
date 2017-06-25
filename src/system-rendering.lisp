;;;; ***********************************************************************
;;;;
;;;; Name:          system-rendering.lisp
;;;; Project:       apis: worker bees for application hives
;;;; Purpose:       rendering pages and components
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)


(defmethod render-template ((path pathname) &rest env)
  (emb:execute-emb path :env env))

(defmethod render-template ((path string) &rest env)
  (apply 'render-template (pathname path) env))

;;; (render-template "/Users/mikel/Workshop/src/apis/templates/main.page" :page-title "Hello!")

