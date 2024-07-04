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
   #:*worker-lock*
   #:*last-local-message-delivery*
   #:worker
   #:worker-event-process
   #:worker-id
   #:worker-message-queue
   #:worker-message-ready?
   #:worker-name
   #:worker-running?
   #:define-known-worker
   #:deliver-message
   #:deliver-message-to-worker
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
   #:start-worker
   #:start-messaging
   #:stop-worker
   #:stop-messaging
   #:the-messenger
   ))

