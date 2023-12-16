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
   #:*agent-lock*
   #:*last-local-message-delivery*
   #:agent
   #:agent-message-queue
   #:agent-event-process
   #:agent-message-ready?
   #:agent-running?
   #:define-known-agent
   #:deliver-message
   #:deliver-message-to-agent
   #:envelope
   #:envelope-contents
   #:find-known-agent
   #:handle-message
   #:handle-message-operation
   #:list-known-agents
   #:list-running-agents
   #:makeid
   #:message
   #:message-data
   #:message-operation
   #:message-timestamp
   #:message-time-to-live
   #:messenger
   #:messenger-known-agents
   #:remove-known-agent
   #:run-agent
   #:send-message
   #:singleton-message
   #:start-agent
   #:start-messaging
   #:stop-agent
   #:stop-messaging
   #:the-messenger
   ))

