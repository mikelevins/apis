;;;; ***********************************************************************
;;;;
;;;; Name:          apis.asd
;;;; Project:       the apis message-passing system
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

;;; ---------------------------------------------------------------------
;;; apis system
;;; ---------------------------------------------------------------------
;;;; apis.asd

(asdf:defsystem #:apis
  :serial t
  :description "Apis: swarms of threads and processes passing messages"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0"
  :version (:read-file-form "version.lisp")
  :depends-on (
               :local-time ; [MIT] https://github.com/dlowe-net/local-time?tab=License-1-ov-file#readme
               :queues.simple-cqueue ; [MIT] https://github.com/oconnore/queues 
               )
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             ))))

;;; (asdf:load-system :apis)
