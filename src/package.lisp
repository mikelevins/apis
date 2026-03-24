;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2014-2026 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage #:apis
  (:use #:cl)
  (:export

   ;; IDs
   #:format-id
   #:makeid
   #:parse-id

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
   #:*default-runtime-thread-count*

   ;; Dead letters
   #:*dead-letters*

   ;; Addressing
   #:format-address
   #:parse-address
   #:malformed-address

   ;; Serialization — payload (Stage 1)
   #:serializable-data
   #:serializable-data-class
   #:serialize-payload
   #:deserialize-payload
   #:non-serializable-type
   #:circular-payload

   ;; Runtime — transport registry (Stage 3)
   #:runtime-local-authority
   #:register-transport
   #:find-transport

   ;; Serialization — envelope (Stage 2)
   #:make-envelope
   #:envelope-id
   #:envelope-from
   #:envelope-to
   #:envelope-operation
   #:envelope-timestamp
   #:envelope-time-to-live
   #:envelope-cause
   #:serialize-envelope
   #:serialize-message
   #:deserialize-message
   #:serialize-message-full
   #:deserialize-message-full

   ;; Transport (Stage 3)
   #:transport
   #:transport-authority
   #:transport-local-authority
   #:transport-transforms
   #:transport-send
   #:transport-receive
   #:transport-close
   #:transport-write-bytes
   #:transport-read-bytes
   #:transport-error
   #:transport-error-reason
   #:make-transform
   #:transform-name
   #:transform-apply-fn
   #:transform-reverse-fn
   #:apply-transforms
   #:reverse-transforms
   #:resolve-transforms
   #:frame-message
   #:deframe-message
   #:loopback-transport
   #:deliver-remotely
   #:enrich-from-address

   ;; Encryption transforms (Stage 4)
   #:make-encryption-transform
   #:make-signing-transform
   #:signature-verification-failed

   ;; TCP transport (Stage 5)
   #:tcp-transport
   #:tcp-transport-socket
   #:tcp-transport-stream
   #:connect-tcp
   #:tcp-listener
   #:listener-host
   #:listener-port
   #:listener-transforms
   #:listener-runtime
   #:listener-running-p
   #:start-tcp-listener
   #:stop-tcp-listener

   ;; Runtime-worker (Stage 6)
   #:runtime-worker
   #:runtime-worker-owning-runtime
   #:install-runtime-worker
   #:swap-runtime-worker
   #:register-pending-reply
   #:pending-reply-count
   #:request
   #:apis-version))
