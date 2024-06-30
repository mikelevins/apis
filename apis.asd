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
  :depends-on (
               :closer-mop ; [MIT] https://github.com/pcostanza/closer-mop
               :frugal-uuid ; [MIT] https://github.com/ak-coram/cl-frugal-uuid
               :local-time ; [MIT] https://github.com/dlowe-net/local-time
               :queues.simple-cqueue ; [MIT] https://github.com/oconnore/queues
               )
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "utilities")
                                     (:file "parameters")
                                     (:file "nodeid")
                                     (:file "singleton-class")
                                     (:file "sessionid")
                                     (:file "messageid")
                                     (:file "message")
                                     (:file "worker")
                                     (:file "messenger")))))

;;; (asdf:load-system :apis)
;;; (ql:quickload :apis)
