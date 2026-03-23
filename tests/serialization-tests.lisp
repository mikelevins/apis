;;;; ***********************************************************************
;;;;
;;;; Name:          serialization-tests.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       tests for serialization and deserialization
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis-tests)

;;; =====================================================================
;;; Test helpers
;;; =====================================================================

(defun round-trip (value)
  "Serialize VALUE as a payload and deserialize the result."
  (apis:deserialize-payload (apis:serialize-payload value)))

;;; =====================================================================
;;; Test classes
;;; =====================================================================

(defclass point (apis:serializable-data)
  ((x :initarg :x :accessor point-x :serializable t)
   (y :initarg :y :accessor point-y :serializable t))
  (:metaclass apis:serializable-data-class))

(defclass labeled-point (point)
  ((label :initarg :label :accessor labeled-point-label :serializable t)
   (cache :accessor labeled-point-cache :initform nil))
  (:metaclass apis:serializable-data-class))

(defclass line-segment (apis:serializable-data)
  ((start :initarg :start :accessor line-start :serializable t)
   (end :initarg :end :accessor line-end :serializable t))
  (:metaclass apis:serializable-data-class))

(defclass person (apis:serializable-data)
  ((name :initarg :name :accessor person-name :serializable t)
   (age :initarg :age :accessor person-age :serializable t)
   (tags :initarg :tags :accessor person-tags :initform nil :serializable t))
  (:metaclass apis:serializable-data-class))

;;; =====================================================================
;;; Primitive round trips
;;; =====================================================================

(define-test serialize-integer
  (check-equal 42 (round-trip 42))
  (check-equal -1 (round-trip -1))
  (check-equal 0 (round-trip 0))
  ;; large integer
  (let ((big (expt 2 128)))
    (check-equal big (round-trip big))))

(define-test serialize-float
  (let ((sf 3.14))
    (check-equal sf (round-trip sf)))
  (let ((df 2.718281828d0))
    (check-equal df (round-trip df))))

(define-test serialize-rational
  (check-equal 1/3 (round-trip 1/3))
  (check-equal -7/11 (round-trip -7/11)))

(define-test serialize-string
  (check-equal "hello" (round-trip "hello"))
  (check-equal "" (round-trip ""))
  (check-equal "line one
line two" (round-trip "line one
line two")))

(define-test serialize-keyword
  (check-equal :foo (round-trip :foo))
  (check-equal :greet (round-trip :greet)))

(define-test serialize-boolean
  (check-equal t (round-trip t))
  (check-equal nil (round-trip nil)))

(define-test serialize-symbol
  (let ((result (round-trip 'cl:cons)))
    (check-equal 'cl:cons result
                 "symbol should round-trip with package identity")))

;;; =====================================================================
;;; Collection round trips
;;; =====================================================================

(define-test serialize-list
  (let ((result (round-trip '(1 2 3))))
    (check-equal '(1 2 3) result)))

(define-test serialize-nested-list
  (let ((result (round-trip '(1 (2 3) (4 (5))))))
    (check-equal '(1 (2 3) (4 (5))) result)))

(define-test serialize-vector
  (let ((result (round-trip #(1 2 3))))
    (check-equal 3 (length result))
    (check (vectorp result) "should deserialize to a vector")
    (check-equal 1 (aref result 0))
    (check-equal 2 (aref result 1))
    (check-equal 3 (aref result 2))))

(define-test serialize-empty-vector
  (let ((result (round-trip #())))
    (check (vectorp result) "empty vector should round-trip as vector")
    (check-equal 0 (length result))))

(define-test serialize-mixed-list
  (let ((result (round-trip '(:name "Alice" :age 34 :active t))))
    (check-equal '(:name "Alice" :age 34 :active t) result)))

(define-test serialize-list-of-vectors
  (let* ((data (list #(1 2) #(3 4)))
         (result (round-trip data)))
    (check-equal 2 (length result))
    (check (vectorp (first result)) "elements should be vectors")
    (check-equal 1 (aref (first result) 0))
    (check-equal 4 (aref (second result) 1))))

;;; =====================================================================
;;; Plist round trips (typical message data)
;;; =====================================================================

(define-test serialize-simple-plist
  (let ((data '(:name "Alice" :count 42 :ratio 1/3)))
    (check-equal data (round-trip data))))

(define-test serialize-nil-payload
  (check-equal nil (round-trip nil)))

(define-test serialize-plist-with-nested-list
  (let ((data '(:items (1 2 3) :label "test")))
    (check-equal data (round-trip data))))

;;; =====================================================================
;;; Serializable-data round trips
;;; =====================================================================

(define-test serialize-simple-object
  (let* ((p (make-instance 'point :x 10 :y 20))
         (result (round-trip p)))
    (check (typep result 'point) "should deserialize to a point")
    (check-equal 10 (point-x result))
    (check-equal 20 (point-y result))))

(define-test serialize-object-with-string-slot
  (let* ((p (make-instance 'labeled-point :x 1 :y 2 :label "origin"))
         (result (round-trip p)))
    (check (typep result 'labeled-point)
           "should deserialize to a labeled-point")
    (check-equal 1 (point-x result))
    (check-equal 2 (point-y result))
    (check-equal "origin" (labeled-point-label result))
    ;; cache slot should be nil (re-initialized from default, not serialized)
    (check (null (labeled-point-cache result))
           "non-serializable slot should get its initform default")))

(define-test serialize-nested-objects
  (let* ((p1 (make-instance 'point :x 0 :y 0))
         (p2 (make-instance 'point :x 10 :y 20))
         (seg (make-instance 'line-segment :start p1 :end p2))
         (result (round-trip seg)))
    (check (typep result 'line-segment)
           "should deserialize to a line-segment")
    (check (typep (line-start result) 'point)
           "start should be a point")
    (check (typep (line-end result) 'point)
           "end should be a point")
    (check-equal 0 (point-x (line-start result)))
    (check-equal 0 (point-y (line-start result)))
    (check-equal 10 (point-x (line-end result)))
    (check-equal 20 (point-y (line-end result)))))

(define-test serialize-object-with-list-slot
  (let* ((p (make-instance 'person :name "Bob" :age 30
                                   :tags '(:admin :reviewer)))
         (result (round-trip p)))
    (check (typep result 'person) "should deserialize to a person")
    (check-equal "Bob" (person-name result))
    (check-equal 30 (person-age result))
    (check-equal '(:admin :reviewer) (person-tags result))))

(define-test serialize-object-in-plist
  (let* ((p (make-instance 'point :x 5 :y 10))
         (data (list :location p :label "here"))
         (result (round-trip data)))
    (check-equal "here" (getf result :label))
    (check (typep (getf result :location) 'point)
           "plist value should deserialize to a point")
    (check-equal 5 (point-x (getf result :location)))))

(define-test serialize-list-of-objects
  (let* ((points (list (make-instance 'point :x 1 :y 2)
                       (make-instance 'point :x 3 :y 4)))
         (data (list :points points))
         (result (round-trip data)))
    (let ((pts (getf result :points)))
      (check-equal 2 (length pts))
      (check (every (lambda (p) (typep p 'point)) pts)
             "all elements should be points")
      (check-equal 1 (point-x (first pts)))
      (check-equal 4 (point-y (second pts))))))

;;; =====================================================================
;;; Error cases
;;; =====================================================================

(define-test serialize-rejects-hash-table
  (check-condition apis:non-serializable-type
    (apis:serialize-payload (list :data (make-hash-table)))))

(define-test serialize-rejects-function
  (check-condition apis:non-serializable-type
    (apis:serialize-payload (list :action #'identity))))

(define-test serialize-rejects-unregistered-clos-instance
  ;; A standard-object that does NOT inherit from serializable-data
  (check-condition apis:non-serializable-type
    (apis:serialize-payload (list :worker (make-instance 'apis:worker)))))

(define-test serialize-rejects-circular-list
  (let ((lst (list 1 2 3)))
    (setf (cdr (last lst)) lst)
    (check-condition apis:circular-payload
      (apis:serialize-payload lst))))

(define-test serialize-rejects-shared-cons
  ;; Two plist values pointing to the same cons cell
  (let* ((shared (list 1 2 3))
         (data (list :a shared :b shared)))
    (check-condition apis:circular-payload
      (apis:serialize-payload data))))

;;; =====================================================================
;;; Wire format structure
;;; =====================================================================

(define-test wire-format-preserves-readability
  ;; The serialized string should be readable by the Lisp reader
  (let* ((data '(:name "Alice" :count 42))
         (s (apis:serialize-payload data)))
    (check (stringp s) "serialize-payload should return a string")
    (let ((form (let ((*package* (find-package :apis)))
                  (read-from-string s))))
      (check (consp form) "wire form should be readable as a cons"))))
