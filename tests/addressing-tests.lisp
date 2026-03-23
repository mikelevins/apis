;;;; ***********************************************************************
;;;;
;;;; Name:          addressing-tests.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       tests for URI address parsing and formatting
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis-tests)

;;; =====================================================================
;;; format-address
;;; =====================================================================

(define-test format-address-integer
  (let* ((id (apis:makeid))
         (uri (apis:format-address id)))
    (check (stringp uri) "format-address should return a string")
    (check (eql 0 (search "apis:" uri))
           "local URI should start with apis:")
    (check-equal (+ 5 26) (length uri)
                 "local URI should be apis: + 26-char ULID")))

(define-test format-address-string-passthrough
  (let ((uri "apis://somehost.example.com:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"))
    (check-equal uri (apis:format-address uri)
                 "string address should pass through unchanged")))

;;; =====================================================================
;;; parse-address: local
;;; =====================================================================

(define-test parse-local-address
  (let* ((id (apis:makeid))
         (uri (apis:format-address id)))
    (multiple-value-bind (host port worker-id) (apis:parse-address uri)
      (check (null host) "local address should have nil host")
      (check (null port) "local address should have nil port")
      (check-equal id worker-id
                   "local address should recover the original ULID"))))

;;; =====================================================================
;;; parse-address: remote
;;; =====================================================================

(define-test parse-remote-address-no-port
  (let* ((id (apis:makeid))
         (id-str (apis:format-id id))
         (uri (format nil "apis://somehost.example.com/~A" id-str)))
    (multiple-value-bind (host port worker-id) (apis:parse-address uri)
      (check-equal "somehost.example.com" host)
      (check (null port) "no-port URI should have nil port")
      (check-equal id worker-id))))

(define-test parse-remote-address-with-port
  (let* ((id (apis:makeid))
         (id-str (apis:format-id id))
         (uri (format nil "apis://somehost.example.com:9100/~A" id-str)))
    (multiple-value-bind (host port worker-id) (apis:parse-address uri)
      (check-equal "somehost.example.com" host)
      (check-equal 9100 port)
      (check-equal id worker-id))))

(define-test parse-remote-address-localhost
  (let* ((id (apis:makeid))
         (id-str (apis:format-id id))
         (uri (format nil "apis://localhost:4000/~A" id-str)))
    (multiple-value-bind (host port worker-id) (apis:parse-address uri)
      (check-equal "localhost" host)
      (check-equal 4000 port)
      (check-equal id worker-id))))

;;; =====================================================================
;;; round trips
;;; =====================================================================

(define-test address-round-trip-local
  ;; integer → format-address → parse-address → same integer
  (let* ((id (apis:makeid))
         (uri (apis:format-address id)))
    (multiple-value-bind (host port worker-id) (apis:parse-address uri)
      (declare (ignore host port))
      (check-equal id worker-id))))

(define-test address-round-trip-remote
  ;; Construct a remote URI, parse it, verify components, re-format worker-id
  (let* ((id (apis:makeid))
         (id-str (apis:format-id id))
         (uri (format nil "apis://myhost:8080/~A" id-str)))
    (multiple-value-bind (host port worker-id) (apis:parse-address uri)
      (check-equal "myhost" host)
      (check-equal 8080 port)
      (check-equal id worker-id)
      ;; re-format the worker-id and check it matches
      (check-equal id-str (apis:format-id worker-id)))))

;;; =====================================================================
;;; resolve-deserialized-address
;;; =====================================================================

(define-test resolve-local-address-to-integer
  ;; A local URI string should resolve to an integer ULID
  (let* ((id (apis:makeid))
         (uri (apis:format-address id))
         (result (apis::resolve-deserialized-address uri)))
    (check (integerp result) "local URI should resolve to an integer")
    (check-equal id result)))

(define-test resolve-remote-address-to-string
  ;; A remote URI string should resolve to itself
  (let* ((id (apis:makeid))
         (uri (format nil "apis://remote-host:5000/~A" (apis:format-id id)))
         (result (apis::resolve-deserialized-address uri)))
    (check (stringp result) "remote URI should resolve to a string")
    (check-equal uri result)))

;;; =====================================================================
;;; malformed address detection
;;; =====================================================================

(define-test malformed-address-empty-string
  (check-condition apis:malformed-address
    (apis:parse-address "")))

(define-test malformed-address-wrong-scheme
  (check-condition apis:malformed-address
    (apis:parse-address "http://example.com/foo")))

(define-test malformed-address-scheme-only
  (check-condition apis:malformed-address
    (apis:parse-address "apis:")))

(define-test malformed-address-double-slash-only
  (check-condition apis:malformed-address
    (apis:parse-address "apis://")))

(define-test malformed-address-no-path-separator
  (check-condition apis:malformed-address
    (apis:parse-address "apis://somehost")))

(define-test malformed-address-empty-worker-id
  (check-condition apis:malformed-address
    (apis:parse-address "apis://somehost/")))

(define-test malformed-address-empty-host
  (check-condition apis:malformed-address
    (apis:parse-address "apis://:8080/01ARZ3NDEKTSV4RRFFQ69G5FAV")))

(define-test malformed-address-non-numeric-port
  (check-condition apis:malformed-address
    (apis:parse-address "apis://host:abc/01ARZ3NDEKTSV4RRFFQ69G5FAV")))

(define-test malformed-address-empty-port
  (check-condition apis:malformed-address
    (apis:parse-address "apis://host:/01ARZ3NDEKTSV4RRFFQ69G5FAV")))

(define-test malformed-address-bad-ulid-characters
  (check-condition apis:malformed-address
    (apis:parse-address "apis:NOT-A-VALID-ULID!!!!!!")))

(define-test malformed-address-ulid-too-short
  (check-condition apis:malformed-address
    (apis:parse-address "apis:01ARZ3")))

(define-test malformed-address-not-a-string
  (check-condition apis:malformed-address
    (apis:parse-address 42)))
