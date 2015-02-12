;;;; ***********************************************************************
;;;;
;;;; Name:          stage.asd
;;;; Project:       a presentation server for apis apps
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

;;; ---------------------------------------------------------------------
;;; stage system
;;; ---------------------------------------------------------------------

(asdf:defsystem #:stage
  :serial t
  :description "Stage: presentation for Apis"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0"
  :depends-on (:apis
               :bordeaux-threads
               :singleton-classes)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "stage")
                                     ))))

;;; (asdf:load-system :stage)

