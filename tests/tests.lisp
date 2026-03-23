;;;; ***********************************************************************
;;;;
;;;; Name:          tests.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       test suite for the apis API
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis-tests)

;;; =====================================================================
;;; IDs
;;; =====================================================================

(define-test id-produces-integer
  (check (integerp (apis:makeid)) "makeid should return an integer"))

(define-test id-is-128-bit
  (let ((id (apis:makeid)))
    (check (<= (integer-length id) 128)
           "ULID should fit in 128 bits")))

(define-test id-time-sortable
  (let ((id1 (apis:makeid)))
    (sleep 0.01)
    (let ((id2 (apis:makeid)))
      (check (> id2 id1)
             "later ULID should be greater than earlier ULID"))))

(define-test id-format-length
  (let ((s (apis::format-id (apis:makeid))))
    (check-equal 26 (length s) "formatted ULID should be 26 characters")))

(define-test id-format-parse-round-trip
  (let* ((id (apis:makeid))
         (s (apis::format-id id))
         (id2 (apis::parse-id s)))
    (check-equal id id2 "parse-id should invert format-id")))

(define-test id-parse-invalid-length
  (check-condition error
    (apis::parse-id "tooshort")))

;;; =====================================================================
;;; Messages
;;; =====================================================================

(define-test message-construction
  (let* ((from-id (apis:makeid))
         (to-id (apis:makeid))
         (msg (apis:message :from from-id :to to-id
                            :operation :greet
                            :data '(:name "Alice"))))
    (check (integerp (apis:message-id msg)) "message-id should be an integer")
    (check-equal from-id (apis:message-from msg))
    (check-equal to-id (apis:message-to msg))
    (check-equal :greet (apis:message-operation msg))
    (check-equal '(:name "Alice") (apis:message-data msg))
    (check (integerp (apis:message-timestamp msg)) "timestamp should be an integer")
    (check-equal 600 (apis:message-time-to-live msg))))

(define-test message-defaults
  (let ((msg (apis:message :to (apis:makeid) :operation :test)))
    (check (null (apis:message-from msg)) "from should default to nil")
    (check (null (apis:message-data msg)) "data should default to nil")
    (check (null (apis:message-cause msg)) "cause should default to nil")))

(define-test message-cause-field
  (let* ((cause-id (apis:makeid))
         (msg (apis:message :to (apis:makeid) :operation :ack :cause cause-id)))
    (check-equal cause-id (apis:message-cause msg))))

(define-test message-prints-without-error
  (let ((msg (apis:message :to (apis:makeid) :operation :test)))
    (check (stringp (format nil "~S" msg))
           "printing a message should produce a string")))

;;; =====================================================================
;;; Workers
;;; =====================================================================

(define-test worker-creation
  (let ((w (make-instance 'apis:worker)))
    (check (integerp (apis:worker-id w)) "worker-id should be an integer")
    (check (null (apis:worker-description w)) "description should default to nil")
    (check-equal :idle (apis::worker-state w))))

(define-test worker-description
  (let ((w (make-instance 'apis:worker :description "test worker")))
    (check-equal "test worker" (apis:worker-description w))))

(define-test worker-prints-without-error
  (let ((w (make-instance 'apis:worker)))
    (check (stringp (format nil "~S" w))
           "printing a worker should produce a string")))

(define-test worker-auto-registration
  (let* ((rt apis:*default-runtime*)
         (w (make-instance 'apis:worker))
         (found (apis::find-worker (apis:worker-id w) rt)))
    (check (eq w found) "new worker should be registered in default runtime")))

;;; =====================================================================
;;; handle-message protocol
;;; =====================================================================

(defclass test-worker (apis:worker) ())

(defvar *test-received* nil)

(defmethod apis:handle-message ((w test-worker) msg (op (eql :test-op)) data)
  (setf *test-received* (list (apis:worker-id w) (getf data :value))))

(define-test handle-message-dispatch
  (let* ((w (make-instance 'test-worker))
         (msg (apis:message :to (apis:worker-id w) :operation :test-op
                            :data '(:value 42))))
    (setf *test-received* nil)
    (apis:receive w msg)
    (check (not (null *test-received*)) "handler should have been called")
    (check-equal (apis:worker-id w) (first *test-received*))
    (check-equal 42 (second *test-received*))))

(define-test handle-message-ping
  ;; the built-in :ping handler should complete without signaling an error
  (let* ((w (make-instance 'apis:worker))
         (msg (apis:message :to (apis:worker-id w) :operation :ping))
         (errored nil))
    (handler-case (apis:receive w msg)
      (error () (setf errored t)))
    (check (not errored) ":ping handler should complete without error")))

(define-test unhandled-message-caught-by-receive
  ;; receive catches unhandled-message and returns nil
  (let* ((w (make-instance 'apis:worker))
         (msg (apis:message :to (apis:worker-id w) :operation :no-such-op)))
    (check (null (apis:receive w msg))
           "receive should return nil for unhandled messages")))

;;; =====================================================================
;;; Runtime lifecycle
;;; =====================================================================

(define-test runtime-creation
  (let ((rt (apis:make-runtime :thread-count 2)))
    (check (not (apis:runtime-running-p rt))
           "new runtime should not be running")))

(define-test runtime-start-stop
  (let ((rt (apis:make-runtime :thread-count 2)))
    (apis:start-runtime rt)
    (check (apis:runtime-running-p rt) "runtime should be running after start")
    (apis:stop-runtime rt)
    (check (not (apis:runtime-running-p rt)) "runtime should be stopped after stop")))

(define-test runtime-thread-count-guard
  (let ((rt (apis:make-runtime :thread-count 2)))
    (apis:start-runtime rt)
    ;; attempt to change thread count while running should warn and decline
    (handler-bind ((warning (lambda (w)
                              (declare (ignore w))
                              (muffle-warning))))
      (setf (apis::runtime-thread-count rt) 8))
    (check-equal 2 (apis::runtime-thread-count rt)
                 "thread count should be unchanged")
    (apis:stop-runtime rt)))

(define-test runtime-thread-count-when-stopped
  (let ((rt (apis:make-runtime :thread-count 2)))
    (setf (apis::runtime-thread-count rt) 8)
    (check-equal 8 (apis::runtime-thread-count rt)
                 "thread count should be changeable when stopped")))

;;; =====================================================================
;;; clear-all-queues
;;; =====================================================================

(define-test clear-all-queues-when-stopped
  (let ((rt (apis:make-runtime :thread-count 2)))
    ;; register a worker manually and queue a message
    (let ((w (make-instance 'apis:worker)))
      (apis::register-worker w rt)
      (queues:qpush (apis::worker-message-queue w) :dummy-message)
      (setf (apis::worker-state w) :ready)
      (apis:clear-all-queues rt)
      (check-equal :idle (apis::worker-state w)
                   "worker should be idle after clear")
      (check-equal 0 (queues:qsize (apis::worker-message-queue w))
                   "worker queue should be empty after clear"))))

(define-test clear-all-queues-declined-when-running
  (let ((rt (apis:make-runtime :thread-count 2)))
    (apis:start-runtime rt)
    ;; should warn and return nil
    (let ((warned nil))
      (handler-bind ((warning (lambda (w)
                                (declare (ignore w))
                                (setf warned t)
                                (muffle-warning))))
        (apis:clear-all-queues rt))
      (check warned "clear-all-queues should warn when runtime is running"))
    (apis:stop-runtime rt)))

;;; =====================================================================
;;; send and delivery
;;; =====================================================================

(define-test send-dead-letter-unknown-worker
  (let ((before (length apis:*dead-letters*))
        (msg (apis:message :to (apis:makeid) :operation :test)))
    (apis:send msg)
    (check-equal (1+ before) (length apis:*dead-letters*)
                 "dead letter should be recorded for unknown worker")))

(defvar *delivery-result* nil)

(defmethod apis:handle-message ((w test-worker) msg (op (eql :delivery-test)) data)
  (setf *delivery-result* (getf data :payload)))

(define-test send-delivers-message
  (let ((rt (apis:make-runtime :thread-count 2)))
    (setf apis:*default-runtime* rt)
    (unwind-protect
         (progn
           (setf *delivery-result* nil)
           (let ((w (make-instance 'test-worker)))
             (apis:start-runtime rt)
             (apis:send (apis:message :to (apis:worker-id w)
                                      :operation :delivery-test
                                      :data '(:payload "hello")))
             (sleep 0.2)
             (check-equal "hello" *delivery-result*
                          "handler should have received the payload")
             (apis:stop-runtime rt)))
      ;; restore the default runtime
      (setf apis:*default-runtime*
            (apis:make-runtime :thread-count apis:*default-runtime-thread-count*)))))

;;; =====================================================================
;;; Integration: the smoke test
;;; =====================================================================

(defclass greeter (apis:worker) ())

(defvar *greetings* nil)

(defmethod apis:handle-message ((w greeter) msg (op (eql :greet)) data)
  (push (getf data :name) *greetings*))

(define-test smoke-test
  (let ((rt (apis:make-runtime :thread-count 4)))
    (setf apis:*default-runtime* rt)
    (unwind-protect
         (progn
           (setf *greetings* nil)
           (let ((g1 (make-instance 'greeter))
                 (g2 (make-instance 'greeter)))
             (apis:start-runtime rt)
             (apis:send (apis:message :to (apis:worker-id g1)
                                      :operation :greet
                                      :data '(:name "Alice")))
             (apis:send (apis:message :to (apis:worker-id g2)
                                      :operation :greet
                                      :data '(:name "Bob")))
             (sleep 0.3)
             (apis:stop-runtime rt)
             (check-equal 2 (length *greetings*)
                          "both greetings should have been delivered")
             (check (member "Alice" *greetings* :test #'string=)
                    "Alice should have been greeted")
             (check (member "Bob" *greetings* :test #'string=)
                    "Bob should have been greeted")))
      ;; restore the default runtime
      (setf apis:*default-runtime*
            (apis:make-runtime :thread-count apis:*default-runtime-thread-count*)))))
