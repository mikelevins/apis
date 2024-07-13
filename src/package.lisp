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
  (:use #:cl #:org.tfeb.hax.singleton-classes)
  (:export
   #:handle-message
   #:handle-message-operation
   #:makeid
   #:message
   #:message-arguments
   #:message-operation
   #:message-time-to-live
   #:message-timestamp
   #:send-message
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

