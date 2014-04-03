;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          message.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       representation of messages
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; message
;;; ---------------------------------------------------------------------

(defparameter *default-message-time-to-live* 600) ; seconds

(defclass message ()
  ((id :reader message-id :initform (makeid) :initarg :id)
   (sender-id :accessor sender-id :initform nil :initarg :sender-id)
   (sender-host :accessor sender-host :initform nil :initarg :sender-host)
   (sender-port :accessor sender-port :initform nil :initarg :sender-port)
   (destination-id :accessor destination-id :initform nil :initarg :destination-id)
   (destination-host :accessor destination-host :initform nil :initarg :destination-host)
   (destination-port :accessor destination-port :initform nil :initarg :destination-port)
   (in-response-to :reader in-response-to :initform nil :initarg :in-response-to)
   (timestamp :reader message-timestamp :initform (get-universal-time) :initarg :timestamp)
   (time-to-live :reader message-time-to-live :initform *default-message-time-to-live* :initarg :time-to-live)))

(defun message (message-class-name &rest initargs)
  (apply #'make-instance message-class-name initargs))

(defmethod encode-message ((message message))
  (ccl:with-output-to-vector (out)
    (cl-store:store message out)))

(defmethod decode-message ((message-data vector))
  (ccl:with-input-from-vector (in message-data)
    (cl-store:restore in)))

;;; general requests

(defclass version-request (message)())

(defclass status-request (message)())

;;; a resend request asks a correspondent to resend a
;;; message and supplies the message id of the last
;;; message received.
(defclass resend-request (message)
  ((last-received-message :reader get-last-received-message :initarg :last-received-message)))

;;; general responses

(defclass version-response (message)
  ((version :reader get-version :initarg :version)))

(defclass status-response (message)
  ((status :reader get-status :initarg :status)))

;;; ---------------------------------------------------------------------
;;; secure key exchange
;;; ---------------------------------------------------------------------
;;; Diffie-Hellman key exchange
;;;
;;; either client or server can initiate the exchange. The side that
;;; initiates the exchange is called the Requester; the other side is
;;; called the Responder. 
;;;
;;; The in-response-to slot of the initial start-key-exchange-request is NIL.
;;; In each message of the sequence after that first start-key-exchange-request,
;;; the in-response-to slot carries the message id of the previous message
;;; in the sequence. For example, the in-response-to slot of the 
;;; start-key-exchange-response contains the message id of the initial 
;;; start-key-exchange-request. The next message in sequence, the
;;; key-exchange-encoded-secret-request, contains the message id of the
;;; start-key-exchange-response; and so on to the final message, the
;;; key-exchange-complete-response or key-exchange-failed-response.
;;;
;;; Here's the sequence:
;;;
;;; 1. Requester sends a start-key-exchange-request
;;; 2. Responder sends a start-key-exchange-response, supplying the
;;;    generator g and modulus p
;;; 3. Requestor generates a secret integer a. It then generates a number A
;;;    by calling (dhx::encode-secret a); finally, it sends A to Responder
;;;    in a key-exchange-encoded-secret-request message
;;; 4. Responder generates a secret integer b. It then generates a number B
;;;    by calling (dhx::encode-secret b); finally, it sends B to Requester in
;;;    a key-exchange-encoded-secret-response message. The encoded secret contained
;;;    in this key-exchange-encoded-secret-response is used in the next
;;;    and final exchange; we call this secret the session token.
;;; 5. Requester computes a key k1 by calling (dhx:combine-secrets B a)
;;; 6. Responder computes a key k2 by calling (dhx:combine-secrets A b)
;;;    k1 and k2 are equal, as guaranteed by the properties of the formula
;;;    used to compute them. Requester and Responder now have equal encryption keys.
;;; 7. Requester sends an encrypted key-exchange-complete-request containing the session token.
;;; 8. Responder decrypts the session token from the key-exchange-complete-request and 
;;;    compares it to the session token that it sent to Requester in the previous step.
;;;    if they are equal then the exchange succeeded, and a secure session is established.
;;;    Responder creates a session object representing the secure session and stores it
;;;    using the session token as a key. It then uses the agreed secret key to
;;;    encrypt the session token and sends it back to Requester in a key-exchange-complete-response.
;;;    If the decrypted session token does not match what the Responder sent in the 
;;;    key-exchange-encoded-secret-response step, then the exchange has failed. The Responder
;;;    sends a key-exchange-failed-response message and does not create a secure session.
;;; 9. Requester receives either a key-exchange-complete-response or a key-exchange-failed-response.
;;;    If it receives a key-exchange-failed-response then the exchange failed and no secure session 
;;;    was created. Requester signals an error of type secure-key-exchange-failed. If it receives 
;;;    a key-exchange-complete-response then the exchange has succeeded and a secure session has been 
;;;    created; Requester and Responder can safely and securely exchange necrypted messages.
;;;    In this case Requester creates a secure session object and stores it under the session key
;;;    received from Responder in the key-exchange-encoded-secret-response.

(defclass start-key-exchange-request (message)())

(defclass key-exchange-encoded-secret-request (message)
  ((encoded-secret :reader get-encoded-secret :initarg encoded-secret)))

(defclass key-exchange-complete-request (message)
  ((encrypted-session-token :reader get-encrypted-session-token :initarg :encrypted-session-token)))

(defclass start-key-exchange-response (message)
  ((generator :reader get-generator :initarg :generator)
   (modulus :reader get-modulus :initarg modulus)))

(defclass key-exchange-encoded-secret-response (message)
  ((session-token :reader get-session-token :initarg :session-token))

(defclass key-exchange-complete-response (message)
  ((encrypted-session-token :reader get-encrypted-session-token :initarg :encrypted-session-token)))

(defclass key-exchange-failed-response (message)())

