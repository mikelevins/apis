;;;; ***********************************************************************
;;;;
;;;; Name:          test-framework.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       minimal test framework
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage #:apis-tests
  (:use #:cl)
  (:export #:run-tests
           #:define-test
           #:check
           #:check-equal
           #:check-condition
           ;; Cross-host TCP test helpers (Stage 5)
           #:setup-test-receiver
           #:teardown-test-receiver
           #:send-test-message-tcp
           #:*cross-host-worker*))

(in-package #:apis-tests)

;;; ---------------------------------------------------------------------
;;; test registry
;;; ---------------------------------------------------------------------

(defvar *tests* (make-array 32 :initial-element nil :fill-pointer 0 :adjustable t))

(defstruct test-entry
  name
  function)

(defun register-test (name function)
  (let ((existing (position name *tests* :key #'test-entry-name :test #'string=)))
    (if existing
        (setf (aref *tests* existing) (make-test-entry :name name :function function))
        (vector-push-extend (make-test-entry :name name :function function) *tests*))))

;;; ---------------------------------------------------------------------
;;; defining tests
;;; ---------------------------------------------------------------------

(defmacro define-test (name &body body)
  `(register-test ,(string name)
                  (lambda () ,@body)))

;;; ---------------------------------------------------------------------
;;; assertions
;;; ---------------------------------------------------------------------

(define-condition test-failure (error)
  ((description :initarg :description :reader test-failure-description))
  (:report (lambda (c s) (format s "~A" (test-failure-description c)))))

(defun check (value &optional (description "expected true"))
  "Assert that VALUE is true."
  (unless value
    (error 'test-failure :description description))
  t)

(defun check-equal (expected actual &optional description)
  "Assert that EXPECTED and ACTUAL are EQUAL."
  (unless (equal expected actual)
    (error 'test-failure
           :description (or description
                            (format nil "expected ~S, got ~S" expected actual))))
  t)

(defmacro check-condition (condition-type &body body)
  "Assert that BODY signals a condition of CONDITION-TYPE."
  (let ((ok (gensym "OK")))
    `(let ((,ok nil))
       (handler-case (progn ,@body)
         (,condition-type () (setf ,ok t)))
       (check ,ok ,(format nil "expected condition ~S" condition-type)))))

;;; ---------------------------------------------------------------------
;;; running tests
;;; ---------------------------------------------------------------------

(defun run-tests ()
  "Run all registered tests. Print results and return T if all passed."
  (let ((pass-count 0)
        (fail-count 0)
        (total (length *tests*)))
    (format t "~&Running ~D test~:P...~%~%" total)
    (loop for entry across *tests*
          for name = (test-entry-name entry)
          for fn = (test-entry-function entry)
          do (handler-case
                 (progn (funcall fn)
                        (incf pass-count)
                        (format t "  PASS  ~A~%" name))
               (test-failure (err)
                 (incf fail-count)
                 (format t "  FAIL  ~A: ~A~%" name (test-failure-description err)))
               (error (err)
                 (incf fail-count)
                 (format t "  ERROR ~A: ~A~%" name err))))
    (format t "~%~D passed, ~D failed, ~D total.~%"
            pass-count fail-count total)
    (zerop fail-count)))
