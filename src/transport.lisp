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
;;; Stage 3 uses a flat list of transforms per transport.  Stage 4
;;; will upgrade this to a function (envelope → transform-list) for
;;; per-operation transform policies.

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
               :documentation "Flat list of transform structs, applied in order on send.")
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

(defgeneric transport-send (transport envelope-string payload-string)
  (:documentation "Transform the payload, frame both parts, and send.
The base class method handles the pipeline; subclasses provide I/O
via transport-write-bytes."))

(defgeneric transport-receive (transport)
  (:documentation "Receive framed bytes, deframe, and reverse-transform.
Returns (values envelope-string payload-string).  The base class method
handles the pipeline; subclasses provide I/O via transport-read-bytes."))

;;; --- base class methods ---

(defmethod transport-send ((tr transport) envelope-string payload-string)
  "Pipeline: string→octets, transform payload, frame, write."
  (let* ((envelope-octets (string-to-octets envelope-string))
         (payload-octets  (string-to-octets payload-string))
         (transformed-payload (apply-transforms (transport-transforms tr)
                                                payload-octets))
         (framed (frame-message envelope-octets transformed-payload)))
    (transport-write-bytes tr framed)))

(defmethod transport-receive ((tr transport))
  "Pipeline: read, deframe, reverse-transform payload, octets→string."
  (let ((framed (transport-read-bytes tr)))
    (multiple-value-bind (envelope-octets payload-octets)
        (deframe-message framed)
      (let ((restored-payload (reverse-transforms (transport-transforms tr)
                                                  payload-octets)))
        (values (octets-to-string envelope-octets)
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
      (transport-send transport envelope-string payload-string))))

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
