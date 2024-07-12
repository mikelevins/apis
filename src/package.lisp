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
   #:*last-local-message-delivery*
   #:*worker-lock*
   #:define-known-worker
   #:deliver-message
   #:find-known-worker
   #:handle-message
   #:handle-message-operation
   #:list-published-workers
   #:list-running-workers
   #:list-stopped-workers
   #:makeid
   #:message
   #:message-arguments
   #:message-operation
   #:message-time-to-live
   #:message-timestamp
   #:messenger
   #:messenger-published-workers
   #:remove-known-worker
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

