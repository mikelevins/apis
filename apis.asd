;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          apis.asd
;;;; Project:       the apis message-passing system
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)
#+abcl(require "abcl-contrib")

;;; ---------------------------------------------------------------------
;;; apis system
;;; ---------------------------------------------------------------------
;;;; apis.asd

(asdf:defsystem #:apis
  :serial t
  :description "Apis: swarms of threads and processes passing messages"
  :author "mikel evins <mevins@me.com>"
  :license "MIT"
  :depends-on (:usocket
               :ip-interfaces
               :local-time
               :cl-store
               :cl-base64
               :queues 
               :queues.simple-cqueue
               :flexi-streams
               :singleton-classes)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "identifier")
                             (:file "message")
                             (:file "messenger")
                             (:file "envelope")
                             (:file "agent")
                             ))))

;;; (asdf:load-system :apis)
;;; (ql:quickload :apis)
