;;;; ***********************************************************************
;;;;
;;;; Name:          apis.asd
;;;; Project:       the apis message-passing system
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2014-2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ---------------------------------------------------------------------
;;; apis system
;;; ---------------------------------------------------------------------
;;;; apis.asd

(asdf:defsystem #:apis
  :serial t
  :description "Apis: swarms of threads and processes passing messages"
  :author "mikel evins <mevins@me.com>"
  :license "MIT"
  :version (:read-file-form "version.lisp")
  :depends-on (:local-time ; [MIT] https://github.com/dlowe-net/local-time
               :frugal-uuid ; [MIT] https://github.com/ak-coram/cl-frugal-uuid
               )
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "parameters")
                                     (:file "nodeid")
                                     (:file "sessionid")
                                     (:file "messageid")))))

;;; (asdf:load-system :apis)
;;; (ql:quickload :apis)
