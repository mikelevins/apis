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
  :depends-on (:usocket ; [MIT] https://github.com/usocket/usocket
               :cl-store ; [BSD] https://github.com/skypher/cl-store
               :ironclad ; [BSD] https://github.com/sharplispers/ironclad
               :cl-base64 ; [BSD] https://github.com/darabi/cl-base64
               :queues ; [MIT] https://github.com/oconnore/queues 
               :queues.simple-cqueue ; [MIT] https://github.com/oconnore/queues 
               :flexi-streams ; [BSD] https://github.com/edicl/flexi-streams
               :org.tfeb.hax.singleton-classes ; [MIT] https://github.com/tfeb/tfeb-lisp-hax
               :local-time ; [MIT] https://github.com/dlowe-net/local-time?tab=License-1-ov-file#readme
               :net.bardcode.nodeid ; [Apache 2.0] https://github.com/mikelevins/net.bardcode.nodeid
               :net.bardcode.literals ; [Apache 2.0] https://github.com/mikelevins/net.bardcode.literals
               )
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "parameters")
                             (:file "utils")
                             (:file "identifier")
                             (:file "message")
                             (:file "envelope")
                             (:file "worker")
                             (:file "messenger")
                             ))))

;;; (asdf:load-system :apis)
;;; (ql:quickload :apis)
