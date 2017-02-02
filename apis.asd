;;;; ***********************************************************************
;;;;
;;;; Name:          apis.asd
;;;; Project:       apis: a worker bee for application hives
;;;; Purpose:       apis system definition
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ---------------------------------------------------------------------
;;; system definitions
;;; ---------------------------------------------------------------------

(asdf:defsystem #:apis
    :description "a worker bee for application hives"
    :author "mikel evins <mikel@evins.net>"
    :license "Apache 2.0"
    :serial t
    :depends-on ()
    :components ((:module "src"
                          :serial t
                          :components ((:file "package")
                                       (:file "apis")))))

;;; (asdf:load-system :apis)

