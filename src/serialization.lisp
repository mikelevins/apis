;;;; ***********************************************************************
;;;;
;;;; Name:          serialization.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       payload serialization and deserialization
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; Conditions
;;; ---------------------------------------------------------------------

(define-condition non-serializable-type (error)
  ((value :initarg :value :reader non-serializable-type-value))
  (:report (lambda (c s)
             (format s "Value ~S (type ~S) is not a permitted Apis payload type."
                     (non-serializable-type-value c)
                     (type-of (non-serializable-type-value c))))))

(define-condition circular-payload (error)
  ((object :initarg :object :reader circular-payload-object))
  (:report (lambda (c s)
             (format s "Circular or shared reference detected in payload: ~S"
                     (circular-payload-object c)))))

;;; ---------------------------------------------------------------------
;;; Metaclass: serializable-data-class
;;; ---------------------------------------------------------------------
;;; Supports the :serializable slot option on direct and effective
;;; slot definitions.

(defclass serializable-data-class (standard-class)
  ()
  (:documentation "Metaclass for serializable-data and its subclasses.
Supports the :SERIALIZABLE slot option."))

(defmethod c2mop:validate-superclass ((class serializable-data-class)
                                      (superclass standard-class))
  t)

;;; --- custom slot definition classes ---

(defclass serializable-direct-slot-definition
    (c2mop:standard-direct-slot-definition)
  ((serializable :initarg :serializable
                 :initform nil
                 :reader slot-serializable-p)))

(defclass serializable-effective-slot-definition
    (c2mop:standard-effective-slot-definition)
  ((serializable :initarg :serializable
                 :initform nil
                 :accessor slot-serializable-p)))

(defmethod c2mop:direct-slot-definition-class ((class serializable-data-class)
                                               &rest initargs)
  (declare (ignore initargs))
  (find-class 'serializable-direct-slot-definition))

(defmethod c2mop:effective-slot-definition-class ((class serializable-data-class)
                                                  &rest initargs)
  (declare (ignore initargs))
  (find-class 'serializable-effective-slot-definition))

(defmethod c2mop:compute-effective-slot-definition ((class serializable-data-class)
                                                    name
                                                    direct-slots)
  (declare (ignore name))
  (let ((effective-slot (call-next-method)))
    (when (some #'slot-serializable-p direct-slots)
      (setf (slot-serializable-p effective-slot) t))
    effective-slot))

;;; ---------------------------------------------------------------------
;;; CLASS serializable-data
;;; ---------------------------------------------------------------------

(defclass serializable-data ()
  ()
  (:metaclass serializable-data-class)
  (:documentation "Abstract base class for user-defined serializable
payload types. Subclass this and mark slots with :SERIALIZABLE T to
enable transport across process and host boundaries."))

;;; ---------------------------------------------------------------------
;;; Slot introspection
;;; ---------------------------------------------------------------------

(defun serializable-slot-info (class)
  "Return a list of (slot-name initarg) for all serializable slots of CLASS.
Signals an error if a serializable slot has no initarg."
  (c2mop:ensure-finalized class)
  (loop for slot in (c2mop:class-slots class)
        when (and (typep slot 'serializable-effective-slot-definition)
                  (slot-serializable-p slot))
          collect (let ((name (c2mop:slot-definition-name slot))
                        (initargs (c2mop:slot-definition-initargs slot)))
                    (unless initargs
                      (error "Serializable slot ~S of class ~S has no initarg."
                             name (class-name class)))
                    (list name (first initargs)))))

;;; ---------------------------------------------------------------------
;;; Serialization: Lisp values → wire forms
;;; ---------------------------------------------------------------------
;;; A wire form is a Lisp s-expression that can be printed readably
;;; and read back unambiguously.
;;;
;;; Primitives (integers, floats, rationals, strings, keywords, t, nil)
;;; are self-representing.
;;;
;;; Compound values use keyword tags:
;;;   list   → (:lst elem1 elem2 ...)
;;;   vector → (:vec elem1 elem2 ...)
;;;   symbol → (:sym "PACKAGE-NAME" "SYMBOL-NAME")
;;;   object → (:obj "PACKAGE-NAME" "CLASS-NAME" :initarg1 val1 ...)

(defun serialize-list (value visited)
  "Serialize a proper list to wire form, walking the spine to detect
circular and shared structure. Signals CIRCULAR-PAYLOAD if any cons
cell in the spine has already been visited. Signals
NON-SERIALIZABLE-TYPE if the list is improper (dotted)."
  (let ((result nil)
        (current value))
    (loop while (consp current)
          do (when (gethash current visited)
               (error 'circular-payload :object current))
             (setf (gethash current visited) t)
             (push (serialize-value (car current) visited) result)
             (setf current (cdr current)))
    ;; current should be nil for a proper list
    (when current
      (error 'non-serializable-type :value value))
    (cons :lst (nreverse result))))

(defun serialize-value (value visited)
  "Convert VALUE to its wire form. VISITED is an EQ hash table for
cycle detection on compound values."
  (typecase value
    ;; Booleans (before symbol, since t and nil are symbols)
    ((eql t) t)
    ((eql nil) nil)
    ;; Numeric primitives
    (integer value)
    (float value)
    (rational value)
    ;; Strings
    (string value)
    ;; Keywords (before general symbol check)
    (keyword value)
    ;; Non-keyword symbols
    (symbol
     (list :sym
           (package-name (symbol-package value))
           (symbol-name value)))
    ;; Lists — walk the spine for cycle detection
    (cons
     (serialize-list value visited))
    ;; Vectors (simple one-dimensional arrays)
    ((simple-array * (*))
     (when (gethash value visited)
       (error 'circular-payload :object value))
     (setf (gethash value visited) t)
     (cons :vec (map 'list (lambda (v) (serialize-value v visited))
                     value)))
    ;; Serializable CLOS instances
    (serializable-data
     (when (gethash value visited)
       (error 'circular-payload :object value))
     (setf (gethash value visited) t)
     (let* ((class (class-of value))
            (slot-info (serializable-slot-info class))
            (initargs (loop for (slot-name initarg) in slot-info
                            append (list initarg
                                        (serialize-value
                                         (slot-value value slot-name)
                                         visited)))))
       (list* :obj
              (package-name (symbol-package (class-name class)))
              (symbol-name (class-name class))
              initargs)))
    ;; Anything else is an error
    (t (error 'non-serializable-type :value value))))

;;; ---------------------------------------------------------------------
;;; Deserialization: wire forms → Lisp values
;;; ---------------------------------------------------------------------

(defun deserialize-value (form)
  "Reconstruct a Lisp value from its wire form."
  (cond
    ;; Atoms: self-representing
    ((null form) nil)
    ((eq form t) t)
    ((integerp form) form)
    ((floatp form) form)
    ((rationalp form) form)
    ((stringp form) form)
    ((keywordp form) form)
    ;; Tagged compound forms
    ((consp form)
     (case (car form)
       (:sym
        (destructuring-bind (tag pkg-name sym-name) form
          (declare (ignore tag))
          (let ((pkg (find-package pkg-name)))
            (unless pkg
              (error "Package ~S not found during deserialization." pkg-name))
            (intern sym-name pkg))))
       (:lst
        (mapcar #'deserialize-value (cdr form)))
       (:vec
        (map 'vector #'deserialize-value (cdr form)))
       (:obj
        (destructuring-bind (tag pkg-name class-name &rest initargs) form
          (declare (ignore tag))
          (let* ((pkg (find-package pkg-name))
                 (sym (and pkg (find-symbol class-name pkg)))
                 (class (and sym (find-class sym nil))))
            (unless class
              (error "Class ~A:~A not found during deserialization."
                     pkg-name class-name))
            ;; Deserialize the initarg values
            (let ((deserialized-initargs
                    (loop for (key val) on initargs by #'cddr
                          append (list key (deserialize-value val)))))
              (apply #'make-instance class deserialized-initargs)))))
       (otherwise
        (error "Unknown wire form tag: ~S" (car form)))))
    ;; Symbols that aren't keywords (shouldn't appear bare in wire format,
    ;; but handle defensively)
    ((symbolp form) form)
    (t (error "Unexpected wire form: ~S" form))))

;;; ---------------------------------------------------------------------
;;; Public API
;;; ---------------------------------------------------------------------

(defun serialize-payload (plist)
  "Serialize a message data plist to a string.
Validates that all values are permitted types. Signals
NON-SERIALIZABLE-TYPE if an invalid value is found, or
CIRCULAR-PAYLOAD if circular/shared structure is detected."
  (let ((visited (make-hash-table :test 'eq)))
    (let ((wire-form (serialize-value plist visited)))
      (let ((*print-readably* t)
            (*print-pretty* nil)
            (*package* (find-package :apis)))
        (prin1-to-string wire-form)))))

(defun deserialize-payload (string)
  "Deserialize a message data plist from a string produced by
SERIALIZE-PAYLOAD."
  (let ((*package* (find-package :apis)))
    (let ((wire-form (read-from-string string)))
      (deserialize-value wire-form))))

;;; ---------------------------------------------------------------------
;;; Envelope structure
;;; ---------------------------------------------------------------------
;;; The envelope is a defstruct with :type list, so its runtime
;;; representation IS a list — the wire form printed by the serializer
;;; and recovered by the reader.  Fields carry wire-form values:
;;; ULID strings, URI strings, keywords, and integers.

(defstruct (envelope (:type list))
  id from to operation timestamp time-to-live cause)

;;; ---------------------------------------------------------------------
;;; Envelope serialization
;;; ---------------------------------------------------------------------

(defun serialize-envelope (message)
  "Build an envelope list from MESSAGE, converting all fields to wire form.
IDs become Crockford base32 strings, addresses become URI strings,
operation remains a keyword, timestamp and TTL remain integers."
  (make-envelope
   :id (format-id (message-id message))
   :from (let ((f (message-from message)))
           (when f (format-address f)))
   :to (let ((a (message-to message)))
         (when a (format-address a)))
   :operation (message-operation message)
   :timestamp (message-timestamp message)
   :time-to-live (message-time-to-live message)
   :cause (let ((c (message-cause message)))
            (when c (format-id c)))))

(defun reconstruct-message-from-envelope (envelope data)
  "Build a message from a deserialized ENVELOPE list and DATA plist.
Converts wire-form fields back to rich values: ULID strings become
integers, local URI strings become integer ULIDs, remote URI strings
are preserved."
  (message :id (parse-id (envelope-id envelope))
           :from (let ((f (envelope-from envelope)))
                   (when f (resolve-deserialized-address f)))
           :to (let ((a (envelope-to envelope)))
                 (when a (resolve-deserialized-address a)))
           :operation (envelope-operation envelope)
           :data data
           :timestamp (envelope-timestamp envelope)
           :time-to-live (envelope-time-to-live envelope)
           :cause (let ((c (envelope-cause envelope)))
                    (when c (parse-id c)))))

;;; ---------------------------------------------------------------------
;;; Message serialization: public API
;;; ---------------------------------------------------------------------

(defun serialize-message (message)
  "Serialize MESSAGE into separate envelope and payload strings.
Returns (values envelope-string payload-string).
Validates the payload; signals a condition if the data contains
non-permitted types."
  (let ((envelope (serialize-envelope message))
        (payload-string (serialize-payload (message-data message))))
    (values (let ((*print-readably* t)
                  (*print-pretty* nil)
                  (*package* (find-package :apis)))
              (prin1-to-string envelope))
            payload-string)))

(defun deserialize-message (envelope-string payload-string)
  "Reconstruct a message from its serialized envelope and payload strings."
  (let ((*package* (find-package :apis)))
    (reconstruct-message-from-envelope
     (read-from-string envelope-string)
     (deserialize-payload payload-string))))

(defun serialize-message-full (message)
  "Serialize MESSAGE into a single string containing both envelope
and payload.  The result is a printed two-element list:
  (envelope-form payload-wire-form)
Validates the payload; signals a condition if the data contains
non-permitted types."
  (let* ((envelope (serialize-envelope message))
         (visited (make-hash-table :test 'eq))
         (payload-wire (serialize-value (message-data message) visited)))
    (let ((*print-readably* t)
          (*print-pretty* nil)
          (*package* (find-package :apis)))
      (prin1-to-string (list envelope payload-wire)))))

(defun deserialize-message-full (string)
  "Reconstruct a message from a string produced by SERIALIZE-MESSAGE-FULL."
  (let ((*package* (find-package :apis)))
    (destructuring-bind (envelope payload-wire) (read-from-string string)
      (reconstruct-message-from-envelope
       envelope
       (deserialize-value payload-wire)))))
