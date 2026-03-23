;;;; ***********************************************************************
;;;;
;;;; Name:          apis.asd
;;;; Project:       the apis message-passing system
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2024-2025 by mikel evins
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
  :description "Apis: actors passing messages"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0"
  :version (:read-file-form "version.lisp")
  :depends-on (
               :bordeaux-threads ; [MIT] https://github.com/sionescu/bordeaux-threads
               :queues.simple-cqueue ; [MIT] https://github.com/oconnore/queues 
               )
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "parameters")
                             (:file "id")
                             (:file "message")
                             (:file "worker")
                             (:file "runtime")
                             ))))

#+repl (asdf:load-system :apis)

#+repl apis:*default-runtime*
#+repl (apis:runtime-running-p apis:*default-runtime*)
#+repl (setf (apis::runtime-thread-count apis:*default-runtime*) 8)
#+repl (defclass greeter (apis:worker) ())
#+repl (defmethod apis:handle-message ((w greeter) msg (op (eql :greet)) data)
         (format t "~&Hello, ~A!~%" (getf data :name)))
#+repl (defparameter *g1* (make-instance 'greeter))
#+repl (defparameter *g2* (make-instance 'greeter))
#+repl (apis:start-runtime apis:*default-runtime*)
#+repl (apis:send (apis:message :to (apis:worker-id *g1*)
                                :operation :greet
                                :data '(:name "Alice")))
#+repl (apis:send (apis:message :to (apis:worker-id *g2*)
                                :operation :greet
                                :data '(:name "Bob")))
#+repl (apis:stop-runtime apis:*default-runtime*)
#+repl (describe apis:*default-runtime*)
