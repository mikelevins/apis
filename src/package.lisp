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
  (:use #:cl #:singleton)
  (:export
   #:handle-message
   #:handle-message-operation
   #:message
   #:message-arguments
   #:message-operation
   #:message-time-to-live
   #:message-timestamp
   #:send
   #:singleton-message
   #:start-messaging
   #:start-worker
   #:stop-messaging
   #:stop-worker
   #:the-messenger
   #:worker
   #:worker-id
   #:worker-message-queue
   #:worker-message-ready?
   #:worker-message-thread
   #:worker-name
   #:worker-running?
   ))

