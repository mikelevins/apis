;;;; ***********************************************************************
;;;;
;;;; Name:          deliver.lisp
;;;; Project:       the stage presentation server for apis
;;;; Purpose:       build the stage app
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package "CL-USER")

(load-all-patches)

(defvar *target-application-path* "~/Desktop/Stage.app")

(require :asdf)

(push #P"/Users/mikel/Workshop/programming/apis/" asdf:*central-registry*)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:defsystem #:stage
  :serial t
  :description "Stage: presentation for Apis"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0"
  :depends-on (:apis
               :singleton-classes)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "stage")
                                     ))))

(ql:quickload :singleton-classes)
(asdf:compile-system :stage)
(asdf:load-system :stage)

(deliver 'stage::run-stage-application
         (create-macos-application-bundle *target-application-path*
                                          :document-types nil)
         0
         :interface :capi)

