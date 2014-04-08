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
  :depends-on (:uuid :entity :usocket :cl-prevalence :cl-base64 :ironclad :cl-store :queues 
                     :queues.simple-cqueue :flexi-streams :singleton-classes)
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "absent")
                         (:file "diffie-hellman")
                         (:file "identifier")
                         (:file "message")
                         (:file "envelope")
                         (:file "messenger")
                         (:file "agent")))))

;;; (asdf:load-system :apis)
