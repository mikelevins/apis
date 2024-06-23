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

(require :asdf)

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
               :net.bardcode.ulid ; [Apache 2.0] https://github.com/mikelevins/net.bardcode.ulid
               )
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "parameters")))))

;;; (ql:quickload :apis)
