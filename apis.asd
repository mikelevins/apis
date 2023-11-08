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
  :version "0.2.0"
  :depends-on (:usocket
               :ip-interfaces
               :cl-store
               :cl-base64
               :queues 
               :queues.simple-cqueue
               :flexi-streams
               :singleton-classes
               :local-time
               :net.bardcode.literals)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "parameters")
                             (:file "identifier")
                             (:file "message")
                             (:file "agent")
                             (:file "messenger")
                             (:file "envelope")
                             ))))

;;; (asdf:load-system :apis)
;;; (ql:quickload :apis)
