;;;; ***********************************************************************
;;;;
;;;; Name:          addressing.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       URI address parsing and formatting
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; Conditions
;;; ---------------------------------------------------------------------

(define-condition malformed-address (error)
  ((text :initarg :text :reader malformed-address-text))
  (:report (lambda (c s)
             (format s "Malformed Apis address: ~S" (malformed-address-text c)))))

;;; ---------------------------------------------------------------------
;;; Address formatting
;;; ---------------------------------------------------------------------

(defun format-address (address)
  "Convert an address to its URI string form.
An integer (ULID) produces a local URI: \"apis:WORKERID\".
A string is returned unchanged (assumed to be a valid URI already)."
  (etypecase address
    (integer (concatenate 'string "apis:" (format-id address)))
    (string address)))

;;; ---------------------------------------------------------------------
;;; Address parsing
;;; ---------------------------------------------------------------------

(defun parse-address (uri-string)
  "Parse an Apis URI string into (values host port worker-id).
HOST and PORT are nil for local addresses.  WORKER-ID is always
returned as an integer (ULID).  Signals MALFORMED-ADDRESS on
invalid input.

Accepted forms:
  apis:WORKERID              — local
  apis://host/WORKERID       — remote, default port
  apis://host:port/WORKERID  — remote, explicit port"
  (unless (and (stringp uri-string)
               (>= (length uri-string) 5)
               (string-equal "apis:" uri-string :end2 5))
    (error 'malformed-address :text uri-string))
  (cond
    ;; Remote: apis://authority/worker-id
    ((and (>= (length uri-string) 7)
          (char= #\/ (char uri-string 5))
          (char= #\/ (char uri-string 6)))
     (parse-remote-address uri-string))
    ;; Local: apis:worker-id
    (t
     (parse-local-address uri-string))))

(defun parse-local-address (uri-string)
  "Parse a local Apis URI (apis:WORKERID).  Returns (values nil nil worker-id).
Signals MALFORMED-ADDRESS if the worker-id portion is empty or invalid."
  (let ((id-str (subseq uri-string 5)))
    (when (zerop (length id-str))
      (error 'malformed-address :text uri-string))
    (handler-case (values nil nil (parse-id id-str))
      (error () (error 'malformed-address :text uri-string)))))

(defun parse-remote-address (uri-string)
  "Parse a remote Apis URI (apis://host[:port]/WORKERID).
Returns (values host port worker-id).  Signals MALFORMED-ADDRESS
if any component is missing or invalid."
  (let ((slash-pos (position #\/ uri-string :start 7)))
    (unless slash-pos
      (error 'malformed-address :text uri-string))
    (let* ((authority (subseq uri-string 7 slash-pos))
           (colon-pos (position #\: authority))
           (host (if colon-pos
                     (subseq authority 0 colon-pos)
                     authority))
           (port (when colon-pos
                   (let ((port-str (subseq authority (1+ colon-pos))))
                     (when (zerop (length port-str))
                       (error 'malformed-address :text uri-string))
                     (handler-case (parse-integer port-str)
                       (error () (error 'malformed-address
                                        :text uri-string))))))
           (id-str (subseq uri-string (1+ slash-pos))))
      (when (zerop (length host))
        (error 'malformed-address :text uri-string))
      (when (zerop (length id-str))
        (error 'malformed-address :text uri-string))
      (handler-case (values host port (parse-id id-str))
        (error () (error 'malformed-address :text uri-string))))))

;;; ---------------------------------------------------------------------
;;; Address resolution helpers
;;; ---------------------------------------------------------------------

(defun resolve-deserialized-address (uri-string)
  "Convert a deserialized URI string back to its internal address form.
Local addresses (no authority) become integer ULIDs.
Remote addresses (with authority) remain as URI strings."
  (multiple-value-bind (host port worker-id) (parse-address uri-string)
    (declare (ignore port))
    (if host uri-string worker-id)))
