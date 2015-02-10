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

;;; ---------------------------------------------------------------------
;;; apis system
;;; ---------------------------------------------------------------------
;;;; apis.asd

(asdf:defsystem #:apis
  :serial t
  :description "Apis: swarms of threads and processes passing messages"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0"
  :depends-on (:usocket
               :ironclad
               :uuid
               :cl-base64
               :queues 
               :queues.simple-cqueue
               :cl-store
               :flexi-streams
               :singleton-classes)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "data-absent")
                             (:file "algo-diffie-hellman")
                             (:file "data-identifier")
                             (:file "comm-message")
                             (:file "comm-envelope")
                             (:file "agent-common")))))

;;; (asdf:load-system :apis)
