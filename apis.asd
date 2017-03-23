
;;;; ***********************************************************************
;;;;
;;;; Name:          apis.asd
;;;; Project:       apis: worker bees for application hives
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
    :description "worker bees for application hives"
    :author "mikel evins <mikel@evins.net>"
    :license "Apache 2.0"
    :serial t
    :depends-on (:uiop :flexi-streams :cl-emb :parenscript :hunchentoot)
    :components ((:module "src"
                          :serial t
                          :components ((:file "package")
                                       (:file "system-project")
                                       (:file "system-html")
                                       (:file "system-server")
                                       (:file "apis")))))

;;; (asdf:load-system :apis)

(asdf:defsystem #:build-apis
  :description "build the apis executable"
  :author "mikel evins <mikel@evins.net>"
  :license "Apache 2.0"
  :serial t
  :depends-on (:apis)
  :components ((:module "src"
                        :serial t
                        :components ((:file "build")))))

;;; (build-apis)
