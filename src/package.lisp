;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage #:apis
  (:use #:cl)
  (:export
   #:description
   #:handle-message
   #:id
   #:message
   #:message-data
   #:message-operation
   #:message-queue
   #:message-semaphore
   #:message-thread
   #:receive
   #:running?
   #:send
   #:start
   #:stop
   #:unhandled-message
   #:worker))

