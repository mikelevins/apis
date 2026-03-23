;;;; ***********************************************************************
;;;;
;;;; Name:          id.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       ULID generation
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; Crockford base32 encoding for ULID string representation
;;; ---------------------------------------------------------------------

(defparameter *crockford-base32-alphabet* "0123456789ABCDEFGHJKMNPQRSTVWXYZ")

(defun crockford-decode-char (ch)
  (let ((pos (position (char-upcase ch) *crockford-base32-alphabet*)))
    (or pos (error "Invalid Crockford base32 character: ~S" ch))))

;;; ---------------------------------------------------------------------
;;; Millisecond timestamps
;;; ---------------------------------------------------------------------
;;; Millisecond precision is achieved by anchoring to GET-UNIVERSAL-TIME
;;; once at load time, then deriving all subsequent timestamps from
;;; GET-INTERNAL-REAL-TIME alone. This avoids non-monotonic results
;;; caused by combining two unsynchronized clock sources.

(defconstant +unix-epoch-in-universal-time+ 2208988800
  "The Unix epoch (1970-01-01T00:00:00Z) as a Common Lisp universal time.")

(defvar *time-anchor-internal* (get-internal-real-time)
  "The value of GET-INTERNAL-REAL-TIME at load time, used as the
reference point for elapsed-time computation.")

(defvar *time-anchor-unix-ms*
  (* (- (get-universal-time) +unix-epoch-in-universal-time+) 1000)
  "Unix epoch milliseconds at load time, computed once from
GET-UNIVERSAL-TIME. All subsequent timestamps are derived by adding
elapsed milliseconds from the internal clock to this anchor.")

(defun current-time-in-milliseconds ()
  "Return the current time as milliseconds since the Unix epoch.
Monotonic: derived from a single clock source (GET-INTERNAL-REAL-TIME)
anchored to a one-time calibration against GET-UNIVERSAL-TIME."
  (let ((elapsed-ms (floor (* (- (get-internal-real-time) *time-anchor-internal*) 1000)
                           internal-time-units-per-second)))
    (+ *time-anchor-unix-ms* elapsed-ms)))

;;; ---------------------------------------------------------------------
;;; ULID generation and formatting
;;; ---------------------------------------------------------------------

(defun makeid (&key time random-bits)
  "Generate a fresh ULID. Returns a 128-bit integer: 48 bits of millisecond
timestamp shifted left by 80, ORed with 80 bits of randomness."
  (let ((timestamp (or time (current-time-in-milliseconds)))
        (randomness (or random-bits (random (ash 1 80) *id-random-state*))))
    (+ (ash (ldb (byte 48 0) timestamp) 80)
       (ldb (byte 80 0) randomness))))

(defun format-id (ulid)
  "Format a ULID integer as a 26-character Crockford base32 string."
  (let ((result (make-string 26 :initial-element #\0)))
    (loop for i from 25 downto 0
          for val = ulid then (ash val -5)
          do (setf (char result i)
                   (char *crockford-base32-alphabet* (logand val #x1F))))
    result))

(defun parse-id (string)
  "Parse a 26-character Crockford base32 string into a ULID integer."
  (assert (= 26 (length string)) (string)
          "ULID string must be 26 characters, got ~D" (length string))
  (let ((result 0))
    (loop for ch across string
          do (setf result (+ (ash result 5) (crockford-decode-char ch))))
    result))
