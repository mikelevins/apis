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
               :closer-mop       ; [MIT] https://github.com/pcostanza/closer-mop
               :ironclad          ; [MIT] https://github.com/sharplispers/ironclad
               :queues.simple-cqueue ; [MIT] https://github.com/oconnore/queues
               :usocket           ; [MIT] https://github.com/usocket/usocket
               )
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "parameters")
                             (:file "id")
                             (:file "message")
                             (:file "worker")
                             (:file "addressing")
                             (:file "runtime")
                             (:file "serialization")
                             (:file "transport")
                             (:file "tcp-transport")
                             (:file "runtime-worker")
                             ))))

;;; ---------------------------------------------------------------------
;;; apis test system
;;; ---------------------------------------------------------------------

(asdf:defsystem #:apis/tests
  :serial t
  :description "Apis test suite"
  :depends-on (#:apis)
  :components ((:module "tests"
                :serial t
                :components ((:file "test-framework")
                             (:file "tests")
                             (:file "serialization-tests")
                             (:file "addressing-tests")
                             (:file "envelope-tests")
                             (:file "transport-tests")
                             (:file "encryption-tests")
                             (:file "tcp-tests")
                             (:file "runtime-worker-tests")))))

;;; (asdf:load-system :apis)
;;; (asdf:load-system :apis/tests :force t)
;;; (apis-tests:run-tests)

;;; ---------------------------------------------------------------------
;;; load and test
;;; ---------------------------------------------------------------------

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
