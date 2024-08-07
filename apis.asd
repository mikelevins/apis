;;;; ***********************************************************************
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
               :closer-mop ; [MIT] https://github.com/pcostanza/closer-mop
               :flexi-streams ; [BSD] https://github.com/edicl/flexi-streams
               :ip-interfaces ; [LLGPL] https://github.com/elliottslaughter/ip-interfaces
               :ironclad ; [BSD] https://github.com/sharplispers/ironclad
               :local-time ; [MIT] https://github.com/dlowe-net/local-time?tab=License-1-ov-file#readme
               :net.bardcode.ksuid ; [Apache 2.0] https://github.com/mikelevins/net.bardcode.ksuid
               :net.bardcode.literals ; [Apache 2.0] https://github.com/mikelevins/net.bardcode.literals
               :net.bardcode.nodeid ; [Apache 2.0] https://github.com/mikelevins/net.bardcode.nodeid
               :net.bardcode.singleton ; [Apache 2.0] https://github.com/mikelevins/net.bardcode.singleton
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
                             (:file "ip-addresses")
                             (:file "delivery-address")
                             (:file "message")
                             (:file "logger")
                             (:file "dead-messages")
                             (:file "worker")
                             (:file "send-receive")
                             (:file "relayer")
                             ))))

;;; (asdf:load-system :apis)
;;; (ql:quickload :apis)
