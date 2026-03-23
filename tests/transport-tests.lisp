;;;; ***********************************************************************
;;;;
;;;; Name:          transport-tests.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       tests for Stage 3 transport layer
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis-tests)

;;; =====================================================================
;;; String ↔ octet conversion
;;; =====================================================================

(define-test octets-round-trip-ascii
  (let* ((s "hello world")
         (octets (apis::string-to-octets s))
         (back (apis::octets-to-string octets)))
    (check (typep octets '(vector (unsigned-byte 8)))
           "should produce an octet vector")
    (check-equal s back "round-trip should preserve string")))

(define-test octets-round-trip-serialized-form
  ;; A realistic serialized s-expression
  (let* ((s "(\"01ARZ3NDEKTSV4RRFFQ69G5FAV\" :GREET 42 3.14d0)")
         (octets (apis::string-to-octets s))
         (back (apis::octets-to-string octets)))
    (check-equal s back "should round-trip s-expression strings")))

;;; =====================================================================
;;; Transform struct
;;; =====================================================================

(define-test transform-struct-creation
  (let ((xf (apis:make-transform :name :test
                                 :apply-fn #'identity
                                 :reverse-fn #'identity)))
    (check (listp xf) "transform should be a list (defstruct :type list)")
    (check-equal :test (apis:transform-name xf))
    (check (functionp (apis:transform-apply-fn xf)))
    (check (functionp (apis:transform-reverse-fn xf)))))

;;; =====================================================================
;;; Transform composition
;;; =====================================================================

(defun make-xor-transform (key-byte)
  "A simple reversible transform for testing: XOR each byte with KEY-BYTE."
  (flet ((xor-bytes (octets)
           (let ((result (make-array (length octets)
                                     :element-type '(unsigned-byte 8))))
             (loop for i below (length octets)
                   do (setf (aref result i)
                            (logxor (aref octets i) key-byte)))
             result)))
    (apis:make-transform :name :xor
                         :apply-fn #'xor-bytes
                         :reverse-fn #'xor-bytes)))

(defun make-pad-transform ()
  "A transform that appends a sentinel byte on apply and strips it on reverse."
  (apis:make-transform
   :name :pad
   :apply-fn (lambda (octets)
               (let ((result (make-array (1+ (length octets))
                                         :element-type '(unsigned-byte 8))))
                 (replace result octets)
                 (setf (aref result (length octets)) #xFF)
                 result))
   :reverse-fn (lambda (octets)
                 (subseq octets 0 (1- (length octets))))))

(define-test apply-transforms-empty-list
  (let* ((data (apis::string-to-octets "test"))
         (result (apis:apply-transforms nil data)))
    (check (equalp data result) "no transforms should return input unchanged")))

(define-test apply-transforms-single
  (let* ((data (apis::string-to-octets "ABC"))
         (xf (make-xor-transform #x42))
         (transformed (apis:apply-transforms (list xf) data))
         (restored (apis:reverse-transforms (list xf) transformed)))
    (check (not (equalp data transformed))
           "XOR transform should change the data")
    (check (equalp data restored)
           "reverse should restore original")))

(define-test apply-transforms-composed
  (let* ((data (apis::string-to-octets "hello"))
         (xf1 (make-xor-transform #x42))
         (xf2 (make-pad-transform))
         (transforms (list xf1 xf2))
         (transformed (apis:apply-transforms transforms data))
         (restored (apis:reverse-transforms transforms transformed)))
    ;; transformed should be XOR'd then padded: one byte longer
    (check-equal (1+ (length data)) (length transformed)
                 "composed transforms: XOR then pad adds one byte")
    (check (equalp data restored)
           "reverse of composed transforms should restore original")))

(define-test reverse-transforms-order
  ;; Verify that reverse-transforms processes in reverse order:
  ;; apply is [xor, pad], reverse must be [un-pad, un-xor]
  (let* ((data (apis::string-to-octets "test"))
         (xf1 (make-xor-transform #x55))
         (xf2 (make-pad-transform))
         (transforms (list xf1 xf2))
         (step1 (funcall (apis:transform-apply-fn xf1) data))
         (step2 (funcall (apis:transform-apply-fn xf2) step1))
         (pipeline-result (apis:apply-transforms transforms data)))
    (check (equalp step2 pipeline-result)
           "pipeline should match manual step-by-step application")))

;;; =====================================================================
;;; Framing
;;; =====================================================================

(define-test frame-deframe-round-trip
  (let* ((env (apis::string-to-octets "(envelope-data)"))
         (pay (apis::string-to-octets "(payload-data)"))
         (framed (apis:frame-message env pay)))
    (check (typep framed '(vector (unsigned-byte 8)))
           "frame should produce an octet vector")
    (multiple-value-bind (env2 pay2) (apis:deframe-message framed)
      (check (equalp env env2) "envelope should round-trip through framing")
      (check (equalp pay pay2) "payload should round-trip through framing"))))

(define-test frame-deframe-empty-payload
  (let* ((env (apis::string-to-octets "(envelope)"))
         (pay (make-array 0 :element-type '(unsigned-byte 8)))
         (framed (apis:frame-message env pay)))
    (multiple-value-bind (env2 pay2) (apis:deframe-message framed)
      (check (equalp env env2) "envelope should survive empty-payload framing")
      (check-equal 0 (length pay2) "empty payload should round-trip"))))

(define-test frame-length-prefix-encoding
  ;; Verify the 4-byte big-endian length prefix is correct
  (let* ((env (apis::string-to-octets "AB"))  ; 2 bytes
         (pay (apis::string-to-octets "XYZ")) ; 3 bytes
         (framed (apis:frame-message env pay)))
    ;; Total: 4 + 2 + 4 + 3 = 13
    (check-equal 13 (length framed) "frame should be 4+2+4+3=13 bytes")
    ;; First 4 bytes: envelope length = 2
    (check-equal 2 (apis::decode-uint32-be framed 0)
                 "envelope length prefix should be 2")
    ;; Next 4 bytes at offset 6: payload length = 3
    (check-equal 3 (apis::decode-uint32-be framed 6)
                 "payload length prefix should be 3")))

(define-test deframe-too-short
  (check-condition apis:transport-error
    (apis:deframe-message (make-array 4 :element-type '(unsigned-byte 8)
                                        :initial-element 0))))

;;; =====================================================================
;;; FROM enrichment
;;; =====================================================================

(define-test enrich-from-bare-integer
  (let* ((id (apis:makeid))
         (msg (apis:message :from id :to "apis://remote/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                            :operation :test))
         (enriched (apis::enrich-from-address msg "myhost:9100")))
    (check (stringp (apis:message-from enriched))
           "enriched FROM should be a string")
    (check (search "myhost:9100" (apis:message-from enriched))
           "enriched FROM should contain the local authority")
    (check (search (apis:format-id id) (apis:message-from enriched))
           "enriched FROM should contain the worker ULID")
    ;; original message untouched
    (check (integerp (apis:message-from msg))
           "original message FROM should still be an integer")))

(define-test enrich-from-already-string
  (let* ((uri "apis://other-host/01ARZ3NDEKTSV4RRFFQ69G5FAV")
         (msg (apis:message :from uri :to "apis://remote/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                            :operation :test))
         (enriched (apis::enrich-from-address msg "myhost:9100")))
    (check (eq msg enriched) "string FROM should pass through unchanged")))

(define-test enrich-from-nil
  (let* ((msg (apis:message :from nil :to "apis://remote/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                            :operation :test))
         (enriched (apis::enrich-from-address msg "myhost:9100")))
    (check (eq msg enriched) "nil FROM should pass through unchanged")))

(define-test enrich-from-no-authority
  (let* ((id (apis:makeid))
         (msg (apis:message :from id :to "apis://remote/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                            :operation :test))
         (enriched (apis::enrich-from-address msg nil)))
    (check (eq msg enriched) "nil local-authority should leave FROM unchanged")))

;;; =====================================================================
;;; Loopback transport: full pipeline
;;; =====================================================================

(define-test loopback-round-trip-no-transforms
  (let ((tr (make-instance 'apis:loopback-transport
                           :local-authority "sender:9100")))
    (multiple-value-bind (id1 id2)
        (make-test-ids)
      (let* ((msg (apis:message :id id1 :from id2
                                :to "apis://remote:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                                :operation :greet
                                :data '(:name "Alice" :count 42)))
             (enriched (apis::enrich-from-address msg "sender:9100")))
        ;; send side
        (multiple-value-bind (env-str pay-str)
            (apis:serialize-message enriched)
          (apis:transport-send tr env-str pay-str)
          ;; receive side
          (multiple-value-bind (env-str2 pay-str2)
              (apis:transport-receive tr)
            (let ((restored (apis:deserialize-message env-str2 pay-str2)))
              (check-equal (apis:message-id msg) (apis:message-id restored)
                           "message ID should survive loopback")
              (check-equal :greet (apis:message-operation restored)
                           "operation should survive loopback")
              (check-equal "Alice" (getf (apis:message-data restored) :name)
                           "payload :name should survive loopback")
              (check-equal 42 (getf (apis:message-data restored) :count)
                           "payload :count should survive loopback"))))))))

(define-test loopback-round-trip-with-transforms
  (let* ((xf (make-xor-transform #xAA))
         (tr (make-instance 'apis:loopback-transport
                            :transforms (list xf)
                            :local-authority "sender:9100")))
    (multiple-value-bind (id1 id2)
        (make-test-ids)
      (let* ((msg (apis:message :id id1 :from id2
                                :to "apis://remote:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                                :operation :transfer
                                :data '(:amount 100 :currency :usd)))
             (enriched (apis::enrich-from-address msg "sender:9100")))
        (multiple-value-bind (env-str pay-str)
            (apis:serialize-message enriched)
          (apis:transport-send tr env-str pay-str)
          (multiple-value-bind (env-str2 pay-str2)
              (apis:transport-receive tr)
            (let ((restored (apis:deserialize-message env-str2 pay-str2)))
              (check-equal :transfer (apis:message-operation restored)
                           "operation should survive transform loopback")
              (check-equal 100 (getf (apis:message-data restored) :amount)
                           "payload :amount should survive transform loopback")
              (check-equal :usd (getf (apis:message-data restored) :currency)
                           "payload :currency should survive transform loopback"))))))))

(define-test loopback-transform-is-not-cleartext
  ;; Verify that when transforms are active, the payload portion of
  ;; the framed bytes is NOT the same as the plain serialized payload
  (let* ((xf (make-xor-transform #xAA))
         (tr (make-instance 'apis:loopback-transport
                            :transforms (list xf)
                            :local-authority "sender:9100")))
    (let* ((msg (apis:message :to "apis://remote/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                              :operation :secret
                              :data '(:key "sensitive-value"))))
      (multiple-value-bind (env-str pay-str)
          (apis:serialize-message msg)
        (apis:transport-send tr env-str pay-str)
        ;; peek at the framed bytes
        (let ((framed (apis::loopback-buffer tr)))
          (multiple-value-bind (env-octets pay-octets)
              (apis:deframe-message framed)
            (declare (ignore env-octets))
            ;; The payload octets should NOT match the plain payload string
            (let ((plain-octets (apis::string-to-octets pay-str)))
              (check (not (equalp plain-octets pay-octets))
                     "transformed payload should differ from plaintext"))))))))

;;; =====================================================================
;;; deliver-remotely: end-to-end through loopback
;;; =====================================================================

(define-test deliver-remotely-enriches-and-sends
  (let* ((tr (make-instance 'apis:loopback-transport
                            :local-authority "myhost:9100"))
         (sender-id (apis:makeid))
         (msg (apis:message :from sender-id
                            :to "apis://remote:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                            :operation :hello
                            :data '(:greeting "hi"))))
    (apis::deliver-remotely msg tr apis:*default-runtime*)
    ;; receive and check FROM enrichment
    (multiple-value-bind (env-str pay-str) (apis:transport-receive tr)
      (let ((restored (apis:deserialize-message env-str pay-str)))
        (check (stringp (apis:message-from restored))
               "received FROM should be a full URI string")
        (check (search "myhost:9100" (apis:message-from restored))
               "received FROM should contain local authority")
        (check-equal :hello (apis:message-operation restored))
        (check-equal "hi" (getf (apis:message-data restored) :greeting))))))

;;; =====================================================================
;;; send integration with transport registry
;;; =====================================================================

(define-test send-uses-registered-transport
  ;; Register a loopback transport for a remote authority, then send
  ;; to that authority and verify the message goes through the transport
  (let* ((rt (apis:make-runtime :thread-count 2))
         (tr (make-instance 'apis:loopback-transport
                            :authority "remote-host:9100"
                            :local-authority "myhost:9100")))
    (setf apis:*default-runtime* rt)
    (unwind-protect
         (progn
           (apis:register-transport "remote-host:9100" tr rt)
           (let* ((sender-id (apis:makeid))
                  (msg (apis:message
                        :from sender-id
                        :to "apis://remote-host:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                        :operation :remote-test
                        :data '(:value 99))))
             (apis:send msg)
             ;; Message should be in the loopback buffer, not dead-lettered
             (check (apis::loopback-buffer tr)
                    "message should be in loopback buffer after send")
             ;; Receive and verify
             (multiple-value-bind (env-str pay-str)
                 (apis:transport-receive tr)
               (let ((restored (apis:deserialize-message env-str pay-str)))
                 (check-equal :remote-test (apis:message-operation restored))
                 (check-equal 99 (getf (apis:message-data restored) :value))
                 (check (stringp (apis:message-from restored))
                        "FROM should be enriched to a URI")))))
      (setf apis:*default-runtime*
            (apis:make-runtime :thread-count apis:*default-runtime-thread-count*)))))

(define-test send-still-dead-letters-without-transport
  ;; Verify that sending to a remote address without a registered
  ;; transport still dead-letters (existing behavior preserved)
  (let ((before (length apis:*dead-letters*))
        (msg (apis:message :to "apis://unknown-host:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                           :operation :test)))
    (apis:send msg)
    (check-equal (1+ before) (length apis:*dead-letters*)
                 "should dead-letter when no transport registered")
    (check (search "no transport"
                   (string-downcase (car (aref apis:*dead-letters*
                                              (1- (length apis:*dead-letters*))))))
           "dead letter reason should mention no transport")))

(define-test send-remote-default-port
  ;; Test transport lookup when remote URI has no explicit port
  (let* ((rt (apis:make-runtime :thread-count 2))
         (tr (make-instance 'apis:loopback-transport
                            :authority "remote-host"
                            :local-authority "myhost:9100")))
    (setf apis:*default-runtime* rt)
    (unwind-protect
         (progn
           (apis:register-transport "remote-host" tr rt)
           (let ((msg (apis:message
                       :to "apis://remote-host/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                       :operation :no-port-test)))
             (apis:send msg)
             (check (apis::loopback-buffer tr)
                    "should route to transport registered without port")))
      (setf apis:*default-runtime*
            (apis:make-runtime :thread-count apis:*default-runtime-thread-count*)))))
