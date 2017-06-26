;;;; ***********************************************************************
;;;;
;;;; Name:          routes-main.lisp
;;;; Project:       apis: worker bees for application hives
;;;; Purpose:       main page route
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defroute (:get "/main")(request response)
  (send-response response
                 :body (render-template "/Users/mikel/Workshop/src/apis/templates/main.page"
                                        :page-title "Welcome to Apis!")))
