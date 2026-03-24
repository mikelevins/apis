;;;; ***********************************************************************
;;;;
;;;; Name:          transport.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       transport layer, transform pipeline, and framing
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; Conditions
;;; ---------------------------------------------------------------------

(define-condition transport-error (error)
  ((reason :initarg :reason :reader transport-error-reason))
  (:report (lambda (c s)
             (format s "Transport error: ~A" (transport-error-reason c)))))

(define-condition signature-verification-failed (transport-error)
  ()
  (:report (lambda (c s)
             (format s "Signature verification failed: ~A"
                     (transport-error-reason c)))))

;;; ---------------------------------------------------------------------
;;; String ↔ octet conversion
;;; ---------------------------------------------------------------------
;;; The serialized strings produced by serialize-message are ASCII-safe
;;; (PRIN1-TO-STRING with *PRINT-READABLY* T escapes non-ASCII chars).
;;; These functions use char-code / code-char which is correct for the
;;; ASCII subset.  If a future binary format introduces raw UTF-8, a
;;; proper encoder should replace these.

(defun string-to-octets (string)
  "Convert STRING to an octet vector.  Assumes ASCII-safe content
(as produced by the Apis serializer)."
  (let ((octets (make-array (length string)
                            :element-type '(unsigned-byte 8))))
    (loop for i below (length string)
          do (setf (aref octets i) (char-code (char string i))))
    octets))

(defun octets-to-string (octets)
  "Convert an octet vector back to a string.  Assumes ASCII-safe content."
  (let ((string (make-string (length octets))))
    (loop for i below (length octets)
          do (setf (char string i) (code-char (aref octets i))))
    string))

;;; ---------------------------------------------------------------------
;;; Transform struct
;;; ---------------------------------------------------------------------
;;; A transform is a named pair of functions (apply / reverse) that
;;; operate on octet vectors.  Transforms compose: the output of one
;;; is the input of the next.
;;;
;;; The transforms slot on a transport may be a flat list of transforms
;;; (applied to every message) or a function (operation-keyword →
;;; transform-list) for per-operation policies.  See RESOLVE-TRANSFORMS.

(defstruct (transform (:type list))
  "A named, reversible byte-to-byte transform."
  name       ; keyword — for logging / debugging
  apply-fn   ; (function ((vector (unsigned-byte 8))) (vector (unsigned-byte 8)))
  reverse-fn); (function ((vector (unsigned-byte 8))) (vector (unsigned-byte 8)))

(defun apply-transforms (transforms bytes)
  "Apply a list of transforms to BYTES in order, returning the result.
Each transform's apply-fn is called on the output of the previous one."
  (reduce (lambda (b xform)
            (funcall (transform-apply-fn xform) b))
          transforms
          :initial-value bytes))

(defun reverse-transforms (transforms bytes)
  "Reverse a list of transforms on BYTES (in reverse order).
Each transform's reverse-fn is called on the output of the previous one."
  (reduce (lambda (b xform)
            (funcall (transform-reverse-fn xform) b))
          (reverse transforms)
          :initial-value bytes))

;;; ---------------------------------------------------------------------
;;; Per-operation transform resolution
;;; ---------------------------------------------------------------------
;;; The transforms slot on a transport may be a flat list (every
;;; message gets the same transforms) or a function (operation-keyword
;;; → transform-list) for per-operation policies.

(defun resolve-transforms (transport operation)
  "Return the transform list for TRANSPORT given OPERATION.
If the transport's transforms slot is a list, return it directly.
If it is a function, call it with OPERATION to get the per-message
transform list."
  (let ((transforms (transport-transforms transport)))
    (etypecase transforms
      (list transforms)
      (function (funcall transforms operation)))))

;;; ---------------------------------------------------------------------
;;; Encryption transform (AES-256-CTR)
;;; ---------------------------------------------------------------------
;;; The apply-fn generates a fresh random 16-byte IV, encrypts with
;;; AES-256 in CTR mode, and prepends the IV to the ciphertext.
;;; The reverse-fn extracts the IV prefix and decrypts.
;;;
;;; KEY must be a 32-byte octet vector for AES-256.

(defun make-encryption-transform (key &key (cipher :aes) (mode :ctr))
  "Return a transform that encrypts on apply and decrypts on reverse.
KEY is an octet vector of appropriate length for CIPHER (32 bytes for
AES-256).  Each encryption uses a fresh random IV, prepended to the
ciphertext so the receiver can extract it."
  (let ((iv-length (ironclad:block-length cipher)))
    (make-transform
     :name :encrypt
     :apply-fn
     (lambda (plaintext)
       (let* ((iv (ironclad:random-data iv-length))
              (c (ironclad:make-cipher cipher :mode mode
                                              :key key
                                              :initialization-vector iv))
              (ciphertext (make-array (length plaintext)
                                     :element-type '(unsigned-byte 8))))
         (ironclad:encrypt c plaintext ciphertext)
         ;; Prepend IV to ciphertext
         (let ((result (make-array (+ iv-length (length ciphertext))
                                   :element-type '(unsigned-byte 8))))
           (replace result iv)
           (replace result ciphertext :start1 iv-length)
           result)))
     :reverse-fn
     (lambda (ciphertext-with-iv)
       (handler-case
           (let* ((iv (subseq ciphertext-with-iv 0 iv-length))
                  (ciphertext (subseq ciphertext-with-iv iv-length))
                  (c (ironclad:make-cipher cipher :mode mode
                                                  :key key
                                                  :initialization-vector iv))
                  (plaintext (make-array (length ciphertext)
                                        :element-type '(unsigned-byte 8))))
             (ironclad:decrypt c ciphertext plaintext)
             plaintext)
         (error (e)
           (error 'transport-error
                  :reason (format nil "Decryption failed: ~A" e))))))))

;;; ---------------------------------------------------------------------
;;; Signing transform (HMAC)
;;; ---------------------------------------------------------------------
;;; The apply-fn computes HMAC(key, bytes) and appends the digest.
;;; The reverse-fn splits off the trailing digest, recomputes, and
;;; verifies with constant-time comparison.
;;;
;;; Composition convention: when using both signing and encryption,
;;; place the signing transform BEFORE the encryption transform in
;;; the transform list.  This means apply does sign-then-encrypt
;;; (the signature covers the plaintext), and reverse does
;;; decrypt-then-verify (reverse order).  This is the standard
;;; authenticated encryption pattern: the signature is protected by
;;; encryption, and verification operates on the original plaintext.

(defun constant-time-equal (a b)
  "Compare octet vectors A and B in constant time.
Returns T if they are the same length and contain identical bytes.
The comparison examines every byte regardless of mismatches, preventing
timing side-channels."
  (if (/= (length a) (length b))
      nil
      (zerop (reduce #'logior
                     (map '(vector (unsigned-byte 8))
                          #'logxor a b)
                     :initial-value 0))))

(defun make-signing-transform (key &key (digest :sha256))
  "Return a transform that appends an HMAC signature on apply and
verifies-then-strips on reverse.  KEY is an octet vector.
Signals SIGNATURE-VERIFICATION-FAILED if verification fails."
  (let ((digest-length (ironclad:digest-length digest)))
    (make-transform
     :name :sign
     :apply-fn
     (lambda (bytes)
       (let* ((hmac (ironclad:make-hmac key digest))
              (_ (ironclad:update-hmac hmac bytes))
              (sig (ironclad:hmac-digest hmac))
              (result (make-array (+ (length bytes) digest-length)
                                  :element-type '(unsigned-byte 8))))
         (declare (ignore _))
         (replace result bytes)
         (replace result sig :start1 (length bytes))
         result))
     :reverse-fn
     (lambda (bytes-with-sig)
       (when (< (length bytes-with-sig) digest-length)
         (error 'signature-verification-failed
                :reason "Message too short to contain a signature"))
       (let* ((split (- (length bytes-with-sig) digest-length))
              (payload (subseq bytes-with-sig 0 split))
              (received-sig (subseq bytes-with-sig split))
              (hmac (ironclad:make-hmac key digest))
              (_ (ironclad:update-hmac hmac payload))
              (expected-sig (ironclad:hmac-digest hmac)))
         (declare (ignore _))
         (unless (constant-time-equal received-sig expected-sig)
           (error 'signature-verification-failed
                  :reason "HMAC digest mismatch"))
         payload)))))

;;; ---------------------------------------------------------------------
;;; Envelope operation extraction
;;; ---------------------------------------------------------------------
;;; For per-operation transform policies, the receive side needs the
;;; operation keyword from the cleartext envelope string before
;;; reversing transforms.  The envelope is a small s-expression list;
;;; we parse it with READ-FROM-STRING and use the envelope accessor.

(defun extract-envelope-operation (envelope-string)
  "Extract the operation keyword from a serialized envelope string.
Returns the operation keyword, or NIL if extraction fails."
  (handler-case
      (let ((*package* (find-package :apis)))
        (envelope-operation (read-from-string envelope-string)))
    (error () nil)))

;;; ---------------------------------------------------------------------
;;; Framing protocol
;;; ---------------------------------------------------------------------
;;; A framed message is a single octet vector containing two
;;; length-prefixed parts:
;;;
;;;   [4 bytes: envelope-length N, big-endian uint32]
;;;   [N bytes: envelope octets]
;;;   [4 bytes: payload-length M, big-endian uint32]
;;;   [M bytes: payload octets (post-transform)]
;;;
;;; This format allows the receiver to recover the envelope/payload
;;; boundary without parsing the content.

(defun encode-uint32-be (n)
  "Encode N as a 4-byte big-endian octet vector."
  (let ((v (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref v 0) (ldb (byte 8 24) n)
          (aref v 1) (ldb (byte 8 16) n)
          (aref v 2) (ldb (byte 8  8) n)
          (aref v 3) (ldb (byte 8  0) n))
    v))

(defun decode-uint32-be (octets offset)
  "Decode a 4-byte big-endian uint32 from OCTETS starting at OFFSET."
  (+ (ash (aref octets (+ offset 0)) 24)
     (ash (aref octets (+ offset 1)) 16)
     (ash (aref octets (+ offset 2))  8)
     (aref octets (+ offset 3))))

(defun frame-message (envelope-octets payload-octets)
  "Combine ENVELOPE-OCTETS and PAYLOAD-OCTETS into a single framed
octet vector with length-prefix framing."
  (let* ((env-len (length envelope-octets))
         (pay-len (length payload-octets))
         (total (+ 4 env-len 4 pay-len))
         (frame (make-array total :element-type '(unsigned-byte 8))))
    ;; envelope: length prefix + data
    (replace frame (encode-uint32-be env-len) :start1 0)
    (replace frame envelope-octets :start1 4)
    ;; payload: length prefix + data
    (let ((pay-offset (+ 4 env-len)))
      (replace frame (encode-uint32-be pay-len) :start1 pay-offset)
      (replace frame payload-octets :start1 (+ pay-offset 4)))
    frame))

(defun deframe-message (framed)
  "Extract envelope and payload octets from a length-prefixed frame.
Returns (values envelope-octets payload-octets)."
  (when (< (length framed) 8)
    (error 'transport-error :reason "Framed message too short"))
  (let* ((env-len (decode-uint32-be framed 0))
         (pay-offset (+ 4 env-len)))
    (when (< (length framed) (+ pay-offset 4))
      (error 'transport-error :reason "Framed message truncated (no payload header)"))
    (let ((pay-len (decode-uint32-be framed pay-offset)))
      (when (< (length framed) (+ pay-offset 4 pay-len))
        (error 'transport-error :reason "Framed message truncated (payload incomplete)"))
      (values (subseq framed 4 (+ 4 env-len))
              (subseq framed (+ pay-offset 4)
                      (+ pay-offset 4 pay-len))))))

;;; ---------------------------------------------------------------------
;;; CLASS transport
;;; ---------------------------------------------------------------------
;;; Base class for all transports.  Owns the transform pipeline and
;;; delegates raw I/O to subclass methods transport-write-bytes and
;;; transport-read-bytes.

(defclass transport ()
  ((authority :reader transport-authority :initarg :authority
              :type string
              :documentation "Remote host or host:port this transport connects to.")
   (transforms :accessor transport-transforms :initarg :transforms
               :initform nil
               :documentation "Transform policy: either a flat list of transform structs
(applied to every message) or a function (operation-keyword → transform-list)
for per-operation policies.  See RESOLVE-TRANSFORMS.")
   (local-authority :reader transport-local-authority :initarg :local-authority
                    :type (or string null)
                    :documentation "This runtime's host:port, for populating FROM addresses.")))

(defmethod print-object ((tr transport) out-stream)
  (print-unreadable-object (tr out-stream :type t :identity nil)
    (format out-stream "~A" (transport-authority tr))))

;;; ---------------------------------------------------------------------
;;; Transport generic protocol
;;; ---------------------------------------------------------------------
;;; Subclasses implement transport-write-bytes and transport-read-bytes
;;; for raw I/O.  The base class methods transport-send and
;;; transport-receive handle the pipeline: transform → frame → write
;;; and read → deframe → reverse-transform.

(defgeneric transport-write-bytes (transport octets)
  (:documentation "Write a framed octet vector to the remote end.
Subclasses must implement this method."))

(defgeneric transport-read-bytes (transport)
  (:documentation "Read a framed octet vector from the remote end.
Subclasses must implement this method.  Returns an octet vector."))

(defgeneric transport-close (transport)
  (:documentation "Release the transport's resources (connections, etc.)."))

(defgeneric transport-send (transport envelope-string payload-string
                            &key operation)
  (:documentation "Transform the payload, frame both parts, and send.
OPERATION is the message operation keyword, used by per-operation
transform policies.  The base class method handles the pipeline;
subclasses provide I/O via transport-write-bytes."))

(defgeneric transport-receive (transport)
  (:documentation "Receive framed bytes, deframe, and reverse-transform.
Returns (values envelope-string payload-string).  The base class method
handles the pipeline; subclasses provide I/O via transport-read-bytes."))

;;; --- base class methods ---

(defmethod transport-send ((tr transport) envelope-string payload-string
                           &key operation)
  "Pipeline: string→octets, resolve transforms, transform payload, frame, write."
  (let* ((envelope-octets (string-to-octets envelope-string))
         (payload-octets  (string-to-octets payload-string))
         (transforms (resolve-transforms tr operation))
         (transformed-payload (apply-transforms transforms payload-octets))
         (framed (frame-message envelope-octets transformed-payload)))
    (transport-write-bytes tr framed)))

(defmethod transport-receive ((tr transport))
  "Pipeline: read, deframe, extract operation, resolve + reverse-transform
payload, octets→string."
  (let ((framed (transport-read-bytes tr)))
    (multiple-value-bind (envelope-octets payload-octets)
        (deframe-message framed)
      (let* ((envelope-string (octets-to-string envelope-octets))
             (operation (extract-envelope-operation envelope-string))
             (transforms (resolve-transforms tr operation))
             (restored-payload (reverse-transforms transforms payload-octets)))
        (values envelope-string
                (octets-to-string restored-payload))))))

(defmethod transport-close ((tr transport))
  "Default: no-op.  Subclasses override to release resources."
  (values))

;;; ---------------------------------------------------------------------
;;; FROM address enrichment
;;; ---------------------------------------------------------------------
;;; When a local worker sends to a remote address, the FROM field may
;;; be a bare integer (local ULID).  The remote receiver needs a full
;;; URI to reply.  This function creates a new message with the enriched
;;; FROM address; the original message object is untouched.

(defun enrich-from-address (msg local-authority)
  "Return a message like MSG but with FROM enriched to a full URI
if it is a bare local ULID and LOCAL-AUTHORITY is non-nil.
If FROM is already a string, nil, or LOCAL-AUTHORITY is nil,
returns MSG unchanged."
  (let ((from (message-from msg)))
    (if (and from (integerp from) local-authority)
        (message :id (message-id msg)
                 :from (format nil "apis://~A/~A"
                               local-authority (format-id from))
                 :to (message-to msg)
                 :operation (message-operation msg)
                 :data (message-data msg)
                 :timestamp (message-timestamp msg)
                 :time-to-live (message-time-to-live msg)
                 :cause (message-cause msg))
        msg)))

;;; ---------------------------------------------------------------------
;;; deliver-remotely
;;; ---------------------------------------------------------------------

(defun deliver-remotely (msg transport runtime)
  "Enrich FROM, serialize, and send MSG via TRANSPORT.
This is the remote counterpart to deliver-locally."
  (declare (ignore runtime))
  (let* ((local-auth (transport-local-authority transport))
         (enriched (enrich-from-address msg local-auth)))
    (multiple-value-bind (envelope-string payload-string)
        (serialize-message enriched)
      (transport-send transport envelope-string payload-string
                      :operation (message-operation enriched)))))

;;; ---------------------------------------------------------------------
;;; CLASS loopback-transport
;;; ---------------------------------------------------------------------
;;; A transport that stores framed bytes in memory rather than sending
;;; them over the network.  Used for pipeline testing.

(defclass loopback-transport (transport)
  ((buffer :accessor loopback-buffer :initform nil
           :documentation "Holds the most recently written framed octets."))
  (:default-initargs :authority "loopback" :local-authority "localhost:9100"))

(defmethod transport-write-bytes ((tr loopback-transport) octets)
  "Store framed bytes in the loopback buffer."
  (setf (loopback-buffer tr) octets))

(defmethod transport-read-bytes ((tr loopback-transport))
  "Return and clear the loopback buffer."
  (let ((data (loopback-buffer tr)))
    (unless data
      (error 'transport-error :reason "Loopback buffer is empty"))
    (setf (loopback-buffer tr) nil)
    data))
