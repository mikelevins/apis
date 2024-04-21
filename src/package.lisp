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
   #:*agent-lock*
   #:*last-local-message-delivery*
   #:agent
   #:agent-event-process
   #:agent-id
   #:agent-message-queue
   #:agent-message-ready?
   #:agent-name
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
   #:list-stopped-agents
   #:makeid
   #:message
   #:message-data
   #:message-operation
   #:message-time-to-live
   #:message-timestamp
   #:messenger
   #:messenger-known-agents
   #:remove-known-agent
   #:send-message
   #:singleton-message
   #:start-agent
   #:start-messaging
   #:stop-agent
   #:stop-messaging
   #:the-messenger
   ))

