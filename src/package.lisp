;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2014-2025 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage #:apis
  (:use #:cl)
  (:export

   ;; IDs
   #:makeid

   ;; Messages
   #:message
   #:message-id
   #:message-from
   #:message-to
   #:message-operation
   #:message-data
   #:message-timestamp
   #:message-time-to-live
   #:message-cause

   ;; Workers
   #:worker
   #:worker-id
   #:worker-description
   #:handle-message
   #:receive
   #:unhandled-message

   ;; Sending
   #:send

   ;; Runtime
   #:runtime
   #:make-runtime
   #:start-runtime
   #:stop-runtime
   #:runtime-running-p
   #:clear-all-queues
   #:*default-runtime*

   ;; Dead letters
   #:*dead-letters*))
