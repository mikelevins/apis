;;;; ***********************************************************************
;;;;
;;;; Name:          encryption-tests.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       tests for Stage 4 encryption transforms
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis-tests)

;;; =====================================================================
;;; Helper: generate a 32-byte AES-256 key
;;; =====================================================================

(defun make-test-key (&optional (seed 42))
  "Generate a deterministic 32-byte key for testing."
  (let ((key (make-array 32 :element-type '(unsigned-byte 8))))
    (loop for i below 32
          do (setf (aref key i) (mod (+ seed i) 256)))
    key))

;;; =====================================================================
;;; Constant-time comparison
;;; =====================================================================

(define-test constant-time-equal-identical
  (let ((a (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3 4)))
        (b (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3 4))))
    (check (apis::constant-time-equal a b)
           "identical vectors should be equal")))

(define-test constant-time-equal-different
  (let ((a (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3 4)))
        (b (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3 5))))
    (check (not (apis::constant-time-equal a b))
           "different vectors should not be equal")))

(define-test constant-time-equal-length-mismatch
  (let ((a (make-array 3 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3)))
        (b (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3 4))))
    (check (not (apis::constant-time-equal a b))
           "different-length vectors should not be equal")))

;;; =====================================================================
;;; Encryption transform: direct round-trip
;;; =====================================================================

(define-test encryption-transform-round-trip
  (let* ((key (make-test-key))
         (xf (apis:make-encryption-transform key))
         (plaintext (apis::string-to-octets "secret payload data")))
    (let* ((encrypted (funcall (apis:transform-apply-fn xf) plaintext))
           (decrypted (funcall (apis:transform-reverse-fn xf) encrypted)))
      (check (equalp plaintext decrypted)
             "decrypt(encrypt(plaintext)) should equal plaintext"))))

(define-test encryption-ciphertext-differs
  (let* ((key (make-test-key))
         (xf (apis:make-encryption-transform key))
         (plaintext (apis::string-to-octets "secret payload data")))
    (let ((encrypted (funcall (apis:transform-apply-fn xf) plaintext)))
      (check (not (equalp plaintext encrypted))
             "ciphertext should differ from plaintext")
      ;; Ciphertext should be longer (IV prepended)
      (check (> (length encrypted) (length plaintext))
             "ciphertext should be longer than plaintext (IV prepended)"))))

(define-test encryption-fresh-iv-each-call
  ;; Two encryptions of the same plaintext should produce different ciphertext
  (let* ((key (make-test-key))
         (xf (apis:make-encryption-transform key))
         (plaintext (apis::string-to-octets "same input")))
    (let ((enc1 (funcall (apis:transform-apply-fn xf) plaintext))
          (enc2 (funcall (apis:transform-apply-fn xf) plaintext)))
      (check (not (equalp enc1 enc2))
             "two encryptions of same plaintext should differ (fresh IV)"))))

(define-test encryption-wrong-key-signals-error
  (let* ((key1 (make-test-key 42))
         (key2 (make-test-key 99))
         (xf-encrypt (apis:make-encryption-transform key1))
         (xf-decrypt (apis:make-encryption-transform key2))
         (plaintext (apis::string-to-octets "secret data")))
    (let ((encrypted (funcall (apis:transform-apply-fn xf-encrypt) plaintext)))
      ;; CTR mode won't error on decrypt — it produces wrong plaintext.
      ;; Verify the result is NOT the original plaintext.
      (let ((result (funcall (apis:transform-reverse-fn xf-decrypt) encrypted)))
        (check (not (equalp plaintext result))
               "decryption with wrong key should not produce original plaintext")))))

(define-test encryption-empty-payload
  ;; Encryption of empty data should round-trip
  (let* ((key (make-test-key))
         (xf (apis:make-encryption-transform key))
         (plaintext (make-array 0 :element-type '(unsigned-byte 8))))
    (let* ((encrypted (funcall (apis:transform-apply-fn xf) plaintext))
           (decrypted (funcall (apis:transform-reverse-fn xf) encrypted)))
      (check (equalp plaintext decrypted)
             "empty payload should round-trip through encryption"))))

;;; =====================================================================
;;; Signing transform: direct round-trip
;;; =====================================================================

(define-test signing-transform-round-trip
  (let* ((key (make-test-key))
         (xf (apis:make-signing-transform key))
         (data (apis::string-to-octets "payload to sign")))
    (let* ((signed (funcall (apis:transform-apply-fn xf) data))
           (verified (funcall (apis:transform-reverse-fn xf) signed)))
      (check (equalp data verified)
             "verify(sign(data)) should return original data"))))

(define-test signing-appends-digest
  (let* ((key (make-test-key))
         (xf (apis:make-signing-transform key))
         (data (apis::string-to-octets "payload"))
         (digest-len (ironclad:digest-length :sha256)))
    (let ((signed (funcall (apis:transform-apply-fn xf) data)))
      (check-equal (+ (length data) digest-len) (length signed)
                   "signed output should be original + digest-length bytes"))))

(define-test signing-tamper-detection
  ;; Flip one byte of the signed payload and verify the signature fails
  (let* ((key (make-test-key))
         (xf (apis:make-signing-transform key))
         (data (apis::string-to-octets "important data")))
    (let ((signed (funcall (apis:transform-apply-fn xf) data)))
      ;; Tamper with the first byte of the payload portion
      (let ((tampered (copy-seq signed)))
        (setf (aref tampered 0) (logxor (aref tampered 0) #xFF))
        (check-condition apis:signature-verification-failed
          (funcall (apis:transform-reverse-fn xf) tampered))))))

(define-test signing-wrong-key-detection
  (let* ((key1 (make-test-key 42))
         (key2 (make-test-key 99))
         (xf-sign (apis:make-signing-transform key1))
         (xf-verify (apis:make-signing-transform key2))
         (data (apis::string-to-octets "payload")))
    (let ((signed (funcall (apis:transform-apply-fn xf-sign) data)))
      (check-condition apis:signature-verification-failed
        (funcall (apis:transform-reverse-fn xf-verify) signed)))))

;;; =====================================================================
;;; Composed sign-then-encrypt
;;; =====================================================================

(define-test sign-then-encrypt-round-trip
  ;; Convention: sign FIRST, encrypt SECOND in the transform list.
  ;; Apply order: sign → encrypt.  Reverse order: decrypt → verify.
  (let* ((enc-key (make-test-key 10))
         (sig-key (make-test-key 20))
         (sign-xf (apis:make-signing-transform sig-key))
         (enc-xf (apis:make-encryption-transform enc-key))
         (transforms (list sign-xf enc-xf))
         (data (apis::string-to-octets "signed and encrypted")))
    (let* ((transformed (apis:apply-transforms transforms data))
           (restored (apis:reverse-transforms transforms transformed)))
      (check (equalp data restored)
             "sign-then-encrypt should round-trip"))))

(define-test sign-then-encrypt-ciphertext-opaque
  ;; The composed output should not contain the plaintext or the signature
  (let* ((enc-key (make-test-key 10))
         (sig-key (make-test-key 20))
         (sign-xf (apis:make-signing-transform sig-key))
         (enc-xf (apis:make-encryption-transform enc-key))
         (transforms (list sign-xf enc-xf))
         (data (apis::string-to-octets "visible string")))
    (let ((transformed (apis:apply-transforms transforms data)))
      (check (not (equalp data (subseq transformed 0 (min (length data)
                                                           (length transformed)))))
             "output should not start with plaintext"))))

;;; =====================================================================
;;; Loopback pipeline with encryption
;;; =====================================================================

(define-test loopback-encryption-round-trip
  (let* ((key (make-test-key))
         (enc-xf (apis:make-encryption-transform key))
         (tr (make-instance 'apis:loopback-transport
                            :transforms (list enc-xf)
                            :local-authority "sender:9100")))
    (multiple-value-bind (id1 id2) (make-test-ids)
      (let* ((msg (apis:message :id id1 :from id2
                                :to "apis://remote:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                                :operation :secret
                                :data '(:key "sensitive-value" :count 7)))
             (enriched (apis::enrich-from-address msg "sender:9100")))
        (multiple-value-bind (env-str pay-str)
            (apis:serialize-message enriched)
          (apis:transport-send tr env-str pay-str :operation :secret)
          (multiple-value-bind (env-str2 pay-str2)
              (apis:transport-receive tr)
            (let ((restored (apis:deserialize-message env-str2 pay-str2)))
              (check-equal :secret (apis:message-operation restored)
                           "operation should survive encrypted loopback")
              (check-equal "sensitive-value"
                           (getf (apis:message-data restored) :key)
                           "payload should survive encrypted loopback")
              (check-equal 7
                           (getf (apis:message-data restored) :count)
                           "payload count should survive encrypted loopback"))))))))

(define-test loopback-signing-round-trip
  (let* ((key (make-test-key))
         (sig-xf (apis:make-signing-transform key))
         (tr (make-instance 'apis:loopback-transport
                            :transforms (list sig-xf)
                            :local-authority "sender:9100")))
    (multiple-value-bind (id1 id2) (make-test-ids)
      (let* ((msg (apis:message :id id1 :from id2
                                :to "apis://remote:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                                :operation :audit
                                :data '(:action "transfer" :amount 500)))
             (enriched (apis::enrich-from-address msg "sender:9100")))
        (multiple-value-bind (env-str pay-str)
            (apis:serialize-message enriched)
          (apis:transport-send tr env-str pay-str :operation :audit)
          (multiple-value-bind (env-str2 pay-str2)
              (apis:transport-receive tr)
            (let ((restored (apis:deserialize-message env-str2 pay-str2)))
              (check-equal :audit (apis:message-operation restored))
              (check-equal "transfer"
                           (getf (apis:message-data restored) :action)))))))))

(define-test loopback-sign-then-encrypt-round-trip
  (let* ((enc-key (make-test-key 10))
         (sig-key (make-test-key 20))
         (sig-xf (apis:make-signing-transform sig-key))
         (enc-xf (apis:make-encryption-transform enc-key))
         (tr (make-instance 'apis:loopback-transport
                            :transforms (list sig-xf enc-xf)
                            :local-authority "sender:9100")))
    (multiple-value-bind (id1 id2) (make-test-ids)
      (let* ((msg (apis:message :id id1 :from id2
                                :to "apis://remote:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                                :operation :payment
                                :data '(:amount 1000 :currency :usd)))
             (enriched (apis::enrich-from-address msg "sender:9100")))
        (multiple-value-bind (env-str pay-str)
            (apis:serialize-message enriched)
          (apis:transport-send tr env-str pay-str :operation :payment)
          (multiple-value-bind (env-str2 pay-str2)
              (apis:transport-receive tr)
            (let ((restored (apis:deserialize-message env-str2 pay-str2)))
              (check-equal :payment (apis:message-operation restored))
              (check-equal 1000
                           (getf (apis:message-data restored) :amount))
              (check-equal :usd
                           (getf (apis:message-data restored) :currency)))))))))

;;; =====================================================================
;;; Per-operation transform policies
;;; =====================================================================

(define-test per-operation-policy-encrypted-operation
  ;; :payment gets sign+encrypt, :ping gets sign-only.
  ;; Test the :payment path.
  (let* ((enc-key (make-test-key 10))
         (sig-key (make-test-key 20))
         (sig-xf (apis:make-signing-transform sig-key))
         (enc-xf (apis:make-encryption-transform enc-key))
         (policy (lambda (operation)
                   (case operation
                     (:payment (list sig-xf enc-xf))
                     (otherwise (list sig-xf)))))
         (tr (make-instance 'apis:loopback-transport
                            :transforms policy
                            :local-authority "sender:9100")))
    (multiple-value-bind (id1 id2) (make-test-ids)
      (let* ((msg (apis:message :id id1 :from id2
                                :to "apis://remote:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                                :operation :payment
                                :data '(:amount 5000 :currency :eur)))
             (enriched (apis::enrich-from-address msg "sender:9100")))
        (multiple-value-bind (env-str pay-str)
            (apis:serialize-message enriched)
          (apis:transport-send tr env-str pay-str :operation :payment)
          ;; Verify payload is not cleartext in the framed bytes
          (let ((framed (apis::loopback-buffer tr)))
            (multiple-value-bind (env-octets pay-octets)
                (apis:deframe-message framed)
              (declare (ignore env-octets))
              (let ((plain-octets (apis::string-to-octets pay-str)))
                (check (not (equalp plain-octets pay-octets))
                       ":payment payload should be encrypted on the wire"))))
          ;; Now receive and verify round-trip
          (multiple-value-bind (env-str2 pay-str2)
              (apis:transport-receive tr)
            (let ((restored (apis:deserialize-message env-str2 pay-str2)))
              (check-equal :payment (apis:message-operation restored))
              (check-equal 5000
                           (getf (apis:message-data restored) :amount)))))))))

(define-test per-operation-policy-signed-only-operation
  ;; Same policy, but test the :ping path (sign-only, no encryption)
  (let* ((enc-key (make-test-key 10))
         (sig-key (make-test-key 20))
         (sig-xf (apis:make-signing-transform sig-key))
         (enc-xf (apis:make-encryption-transform enc-key))
         (policy (lambda (operation)
                   (case operation
                     (:payment (list sig-xf enc-xf))
                     (otherwise (list sig-xf)))))
         (tr (make-instance 'apis:loopback-transport
                            :transforms policy
                            :local-authority "sender:9100")))
    (multiple-value-bind (id1 id2) (make-test-ids)
      (let* ((msg (apis:message :id id1 :from id2
                                :to "apis://remote:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                                :operation :ping
                                :data '(:seq 1)))
             (enriched (apis::enrich-from-address msg "sender:9100")))
        (multiple-value-bind (env-str pay-str)
            (apis:serialize-message enriched)
          (apis:transport-send tr env-str pay-str :operation :ping)
          (multiple-value-bind (env-str2 pay-str2)
              (apis:transport-receive tr)
            (let ((restored (apis:deserialize-message env-str2 pay-str2)))
              (check-equal :ping (apis:message-operation restored))
              (check-equal 1
                           (getf (apis:message-data restored) :seq)))))))))

(define-test per-operation-policy-different-ops-same-transport
  ;; Send two messages with different operations through the same
  ;; policy transport, verify both round-trip correctly
  (let* ((enc-key (make-test-key 10))
         (sig-key (make-test-key 20))
         (sig-xf (apis:make-signing-transform sig-key))
         (enc-xf (apis:make-encryption-transform enc-key))
         (policy (lambda (operation)
                   (case operation
                     (:payment (list sig-xf enc-xf))
                     (otherwise (list sig-xf)))))
         (tr (make-instance 'apis:loopback-transport
                            :transforms policy
                            :local-authority "sender:9100")))
    (multiple-value-bind (id1 id2) (make-test-ids)
      ;; First: a :payment message (encrypted + signed)
      (let* ((pay-msg (apis:message :id id1 :from id2
                                    :to "apis://remote:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                                    :operation :payment
                                    :data '(:amount 100)))
             (enriched (apis::enrich-from-address pay-msg "sender:9100")))
        (multiple-value-bind (env-str pay-str)
            (apis:serialize-message enriched)
          (apis:transport-send tr env-str pay-str :operation :payment)
          (multiple-value-bind (env-str2 pay-str2)
              (apis:transport-receive tr)
            (let ((restored (apis:deserialize-message env-str2 pay-str2)))
              (check-equal :payment (apis:message-operation restored))
              (check-equal 100
                           (getf (apis:message-data restored) :amount))))))
      ;; Second: a :ping message (signed only)
      (let* ((id3 (apis:makeid))
             (ping-msg (apis:message :id id3 :from id2
                                     :to "apis://remote:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                                     :operation :ping
                                     :data '(:seq 42)))
             (enriched (apis::enrich-from-address ping-msg "sender:9100")))
        (multiple-value-bind (env-str pay-str)
            (apis:serialize-message enriched)
          (apis:transport-send tr env-str pay-str :operation :ping)
          (multiple-value-bind (env-str2 pay-str2)
              (apis:transport-receive tr)
            (let ((restored (apis:deserialize-message env-str2 pay-str2)))
              (check-equal :ping (apis:message-operation restored))
              (check-equal 42
                           (getf (apis:message-data restored) :seq)))))))))

;;; =====================================================================
;;; resolve-transforms backward compatibility
;;; =====================================================================

(define-test resolve-transforms-flat-list
  ;; A flat list of transforms should be returned as-is
  (let* ((xf (apis:make-transform :name :dummy
                                  :apply-fn #'identity
                                  :reverse-fn #'identity))
         (tr (make-instance 'apis:loopback-transport
                            :transforms (list xf))))
    (let ((resolved (apis:resolve-transforms tr :anything)))
      (check-equal 1 (length resolved))
      (check (eq xf (first resolved))
             "flat list should be returned unchanged"))))

(define-test resolve-transforms-function
  ;; A function should be called with the operation
  (let* ((xf-a (apis:make-transform :name :a
                                    :apply-fn #'identity
                                    :reverse-fn #'identity))
         (xf-b (apis:make-transform :name :b
                                    :apply-fn #'identity
                                    :reverse-fn #'identity))
         (policy (lambda (op)
                   (case op
                     (:alpha (list xf-a))
                     (otherwise (list xf-b)))))
         (tr (make-instance 'apis:loopback-transport
                            :transforms policy)))
    (let ((for-alpha (apis:resolve-transforms tr :alpha))
          (for-other (apis:resolve-transforms tr :beta)))
      (check (eq xf-a (first for-alpha))
             "should return :a transform for :alpha")
      (check (eq xf-b (first for-other))
             "should return :b transform for other ops"))))

(define-test resolve-transforms-nil-list
  ;; NIL (empty list) is valid — no transforms
  (let ((tr (make-instance 'apis:loopback-transport :transforms nil)))
    (let ((resolved (apis:resolve-transforms tr :anything)))
      (check (null resolved) "nil transforms should resolve to nil"))))
