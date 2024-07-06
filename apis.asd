;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          apis.asd
;;;; Project:       the apis message-passing system
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
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
  :version (:read-file-form "version.lisp")
  :depends-on (
               :cl-base64 ; [BSD] https://github.com/darabi/cl-base64
               :cl-store ; [BSD] https://github.com/skypher/cl-store
               :flexi-streams ; [BSD] https://github.com/edicl/flexi-streams
               :ironclad ; [BSD] https://github.com/sharplispers/ironclad
               :local-time ; [MIT] https://github.com/dlowe-net/local-time?tab=License-1-ov-file#readme
               :net.bardcode.literals ; [Apache 2.0] https://github.com/mikelevins/net.bardcode.literals
               :net.bardcode.nodeid ; [Apache 2.0] https://github.com/mikelevins/net.bardcode.nodeid
               :org.tfeb.hax.singleton-classes ; [MIT] https://github.com/tfeb/tfeb-lisp-hax
               :osicat ; [MIT] https://github.com/osicat/osicat
               :queues ; [MIT] https://github.com/oconnore/queues 
               :queues.simple-cqueue ; [MIT] https://github.com/oconnore/queues 
               :usocket ; [MIT] https://github.com/usocket/usocket
               )
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "parameters")
                             (:file "utils")
                             (:file "identifier")
                             (:file "recipient")
                             (:file "message")
                             (:file "worker")
                             (:file "messenger")
                             ))))

;;; (asdf:load-system :apis)
;;; (ql:quickload :apis)
