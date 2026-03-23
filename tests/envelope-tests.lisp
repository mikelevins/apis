;;;; ***********************************************************************
;;;;
;;;; Name:          envelope-tests.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       tests for envelope and message serialization (Stage 2)
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis-tests)

;;; =====================================================================
;;; Test helpers
;;; =====================================================================

(defun make-test-ids ()
  "Return (values id1 id2 cause-id) — three deterministic ULIDs."
  (values (apis:makeid :time 1700000000000 :random-bits 1)
          (apis:makeid :time 1700000000001 :random-bits 2)
          (apis:makeid :time 1700000000002 :random-bits 3)))

;;; =====================================================================
;;; Envelope structure
;;; =====================================================================

(define-test envelope-is-a-list
  (let ((env (apis:make-envelope :id "X" :from "Y" :to "Z"
                                 :operation :ping
                                 :timestamp 100 :time-to-live 60
                                 :cause nil)))
    (check (listp env) "envelope should be a list")
    (check-equal 7 (length env) "envelope should have 7 elements")))

(define-test envelope-accessors
  (let ((env (apis:make-envelope :id "ID" :from "FROM" :to "TO"
                                 :operation :greet
                                 :timestamp 999 :time-to-live 300
                                 :cause "CAUSE")))
    (check-equal "ID" (apis:envelope-id env))
    (check-equal "FROM" (apis:envelope-from env))
    (check-equal "TO" (apis:envelope-to env))
    (check-equal :greet (apis:envelope-operation env))
    (check-equal 999 (apis:envelope-timestamp env))
    (check-equal 300 (apis:envelope-time-to-live env))
    (check-equal "CAUSE" (apis:envelope-cause env))))

;;; =====================================================================
;;; serialize-envelope
;;; =====================================================================

(define-test serialize-envelope-local-addresses
  (multiple-value-bind (from-id to-id cause-id) (make-test-ids)
    (let* ((msg (apis:message :from from-id :to to-id
                              :operation :greet
                              :data '(:name "Alice")
                              :cause cause-id))
           (env (apis:serialize-envelope msg)))
      (check (listp env) "envelope should be a list")
      ;; id is a 26-char ULID string
      (check (stringp (apis:envelope-id env)) "id should be a string")
      (check-equal 26 (length (apis:envelope-id env)))
      ;; from is a local URI
      (check (stringp (apis:envelope-from env)) "from should be a string")
      (check (eql 0 (search "apis:" (apis:envelope-from env)))
             "from should be an apis: URI")
      ;; to is a local URI
      (check (stringp (apis:envelope-to env)) "to should be a string")
      (check (eql 0 (search "apis:" (apis:envelope-to env)))
             "to should be an apis: URI")
      ;; operation
      (check-equal :greet (apis:envelope-operation env))
      ;; cause is a ULID string
      (check (stringp (apis:envelope-cause env)) "cause should be a string")
      (check-equal 26 (length (apis:envelope-cause env))))))

(define-test serialize-envelope-nil-fields
  (let* ((msg (apis:message :operation :ping))
         (env (apis:serialize-envelope msg)))
    (check (null (apis:envelope-from env)) "nil from should stay nil")
    (check (null (apis:envelope-to env)) "nil to should stay nil")
    (check (null (apis:envelope-cause env)) "nil cause should stay nil")))

(define-test serialize-envelope-remote-address
  (multiple-value-bind (from-id to-id) (make-test-ids)
    (declare (ignore to-id))
    (let* ((remote-uri "apis://remote-host:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV")
           (msg (apis:message :from from-id :to remote-uri
                              :operation :transfer))
           (env (apis:serialize-envelope msg)))
      (check-equal remote-uri (apis:envelope-to env)
                   "remote URI should pass through to envelope"))))

;;; =====================================================================
;;; Envelope wire-form round trip (print/read)
;;; =====================================================================

(define-test envelope-print-read-round-trip
  (multiple-value-bind (from-id to-id cause-id) (make-test-ids)
    (let* ((msg (apis:message :from from-id :to to-id
                              :operation :transfer
                              :data '(:amount 100)
                              :cause cause-id))
           (env (apis:serialize-envelope msg))
           (printed (let ((*print-readably* t)
                          (*print-pretty* nil)
                          (*package* (find-package :apis)))
                      (prin1-to-string env)))
           (recovered (let ((*package* (find-package :apis)))
                        (read-from-string printed))))
      (check-equal (apis:envelope-id env) (apis:envelope-id recovered))
      (check-equal (apis:envelope-from env) (apis:envelope-from recovered))
      (check-equal (apis:envelope-to env) (apis:envelope-to recovered))
      (check-equal (apis:envelope-operation env)
                   (apis:envelope-operation recovered))
      (check-equal (apis:envelope-timestamp env)
                   (apis:envelope-timestamp recovered))
      (check-equal (apis:envelope-time-to-live env)
                   (apis:envelope-time-to-live recovered))
      (check-equal (apis:envelope-cause env)
                   (apis:envelope-cause recovered)))))

;;; =====================================================================
;;; serialize-message / deserialize-message
;;; =====================================================================

(define-test message-round-trip-local
  (multiple-value-bind (from-id to-id cause-id) (make-test-ids)
    (let* ((msg (apis:message :from from-id :to to-id
                              :operation :greet
                              :data '(:name "Alice" :count 42)
                              :cause cause-id)))
      (multiple-value-bind (env-str pay-str) (apis:serialize-message msg)
        (check (stringp env-str) "envelope should be a string")
        (check (stringp pay-str) "payload should be a string")
        (let ((recovered (apis:deserialize-message env-str pay-str)))
          (check-equal (apis:message-id msg) (apis:message-id recovered))
          (check-equal from-id (apis:message-from recovered))
          (check-equal to-id (apis:message-to recovered))
          (check-equal :greet (apis:message-operation recovered))
          (check-equal '(:name "Alice" :count 42)
                       (apis:message-data recovered))
          (check-equal (apis:message-timestamp msg)
                       (apis:message-timestamp recovered))
          (check-equal (apis:message-time-to-live msg)
                       (apis:message-time-to-live recovered))
          (check-equal cause-id (apis:message-cause recovered)))))))

(define-test message-round-trip-remote-addresses
  (multiple-value-bind (from-id to-id) (make-test-ids)
    (declare (ignore to-id))
    (let* ((remote-to "apis://remote-host:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV")
           (msg (apis:message :from from-id :to remote-to
                              :operation :transfer
                              :data '(:amount 500))))
      (multiple-value-bind (env-str pay-str) (apis:serialize-message msg)
        (let ((recovered (apis:deserialize-message env-str pay-str)))
          ;; from was integer (local) → should come back as integer
          (check (integerp (apis:message-from recovered))
                 "local from should deserialize to integer")
          (check-equal from-id (apis:message-from recovered))
          ;; to was remote URI → should come back as string
          (check (stringp (apis:message-to recovered))
                 "remote to should deserialize to string")
          (check-equal remote-to (apis:message-to recovered)))))))

(define-test message-round-trip-nil-fields
  (let* ((msg (apis:message :operation :ping :data nil)))
    (multiple-value-bind (env-str pay-str) (apis:serialize-message msg)
      (let ((recovered (apis:deserialize-message env-str pay-str)))
        (check (null (apis:message-from recovered))
               "nil from should stay nil")
        (check (null (apis:message-to recovered))
               "nil to should stay nil")
        (check (null (apis:message-cause recovered))
               "nil cause should stay nil")
        (check (null (apis:message-data recovered))
               "nil data should stay nil")))))

(define-test message-round-trip-nil-data-with-addresses
  (multiple-value-bind (from-id to-id) (make-test-ids)
    (let* ((msg (apis:message :from from-id :to to-id
                              :operation :ack :data nil)))
      (multiple-value-bind (env-str pay-str) (apis:serialize-message msg)
        (let ((recovered (apis:deserialize-message env-str pay-str)))
          (check-equal from-id (apis:message-from recovered))
          (check-equal to-id (apis:message-to recovered))
          (check-equal :ack (apis:message-operation recovered))
          (check (null (apis:message-data recovered))))))))

(define-test message-round-trip-complex-payload
  (multiple-value-bind (from-id to-id) (make-test-ids)
    (let* ((p (make-instance 'point :x 10 :y 20))
           (msg (apis:message :from from-id :to to-id
                              :operation :update
                              :data (list :location p
                                          :tags '(:urgent :reviewed)
                                          :count 7))))
      (multiple-value-bind (env-str pay-str) (apis:serialize-message msg)
        (let* ((recovered (apis:deserialize-message env-str pay-str))
               (data (apis:message-data recovered)))
          (check-equal :update (apis:message-operation recovered))
          (check-equal 7 (getf data :count))
          (check-equal '(:urgent :reviewed) (getf data :tags))
          (let ((loc (getf data :location)))
            (check (typep loc 'point) "location should be a point")
            (check-equal 10 (point-x loc))
            (check-equal 20 (point-y loc))))))))

(define-test message-round-trip-preserves-timestamp
  (let* ((ts 3953040000)
         (msg (apis:message :operation :ping :timestamp ts
                            :time-to-live 120)))
    (multiple-value-bind (env-str pay-str) (apis:serialize-message msg)
      (let ((recovered (apis:deserialize-message env-str pay-str)))
        (check-equal ts (apis:message-timestamp recovered))
        (check-equal 120 (apis:message-time-to-live recovered))))))

;;; =====================================================================
;;; serialize-message-full / deserialize-message-full
;;; =====================================================================

(define-test message-full-round-trip-local
  (multiple-value-bind (from-id to-id cause-id) (make-test-ids)
    (let* ((msg (apis:message :from from-id :to to-id
                              :operation :greet
                              :data '(:name "Bob" :active t)
                              :cause cause-id))
           (full-str (apis:serialize-message-full msg)))
      (check (stringp full-str) "full serialization should be a string")
      (let ((recovered (apis:deserialize-message-full full-str)))
        (check-equal (apis:message-id msg) (apis:message-id recovered))
        (check-equal from-id (apis:message-from recovered))
        (check-equal to-id (apis:message-to recovered))
        (check-equal :greet (apis:message-operation recovered))
        (check-equal '(:name "Bob" :active t)
                     (apis:message-data recovered))
        (check-equal cause-id (apis:message-cause recovered))))))

(define-test message-full-round-trip-remote
  (let* ((from-uri "apis://origin-host:4000/01ARZ3NDEKTSV4RRFFQ69G5FAV")
         (to-uri "apis://dest-host:5000/01BXQM6K3JANNHPS0BQ0DQZTGV")
         (msg (apis:message :from from-uri :to to-uri
                            :operation :transfer
                            :data '(:amount 1250 :currency :usd)))
         (full-str (apis:serialize-message-full msg)))
    (let ((recovered (apis:deserialize-message-full full-str)))
      ;; remote addresses should come back as strings
      (check (stringp (apis:message-from recovered))
             "remote from should deserialize to string")
      (check-equal from-uri (apis:message-from recovered))
      (check (stringp (apis:message-to recovered))
             "remote to should deserialize to string")
      (check-equal to-uri (apis:message-to recovered))
      (check-equal :transfer (apis:message-operation recovered))
      (check-equal '(:amount 1250 :currency :usd)
                   (apis:message-data recovered)))))

(define-test message-full-round-trip-nil-payload
  (let* ((msg (apis:message :operation :heartbeat :data nil))
         (full-str (apis:serialize-message-full msg)))
    (let ((recovered (apis:deserialize-message-full full-str)))
      (check-equal :heartbeat (apis:message-operation recovered))
      (check (null (apis:message-data recovered))))))

(define-test message-full-round-trip-complex-payload
  (multiple-value-bind (from-id to-id) (make-test-ids)
    (let* ((seg (make-instance 'line-segment
                  :start (make-instance 'point :x 0 :y 0)
                  :end (make-instance 'point :x 100 :y 200)))
           (msg (apis:message :from from-id :to to-id
                              :operation :draw
                              :data (list :shape seg :color :red)))
           (full-str (apis:serialize-message-full msg)))
      (let* ((recovered (apis:deserialize-message-full full-str))
             (data (apis:message-data recovered))
             (shape (getf data :shape)))
        (check (typep shape 'line-segment))
        (check-equal 0 (point-x (line-start shape)))
        (check-equal 200 (point-y (line-end shape)))
        (check-equal :red (getf data :color))))))

;;; =====================================================================
;;; Full-form wire structure
;;; =====================================================================

(define-test message-full-wire-form-is-two-element-list
  (let* ((msg (apis:message :operation :ping :data '(:x 1)))
         (full-str (apis:serialize-message-full msg)))
    (let* ((*package* (find-package :apis))
           (form (read-from-string full-str)))
      (check (consp form) "full wire form should be a cons")
      (check-equal 2 (length form)
                   "full wire form should be a two-element list")
      (check (listp (first form)) "first element should be envelope list")
      (check-equal 7 (length (first form))
                   "envelope should have 7 fields"))))

;;; =====================================================================
;;; Consistency: serialize-message vs serialize-message-full
;;; =====================================================================

(define-test serialize-message-and-full-agree
  ;; Both paths should produce messages that deserialize to equal values
  (multiple-value-bind (from-id to-id) (make-test-ids)
    (let ((msg (apis:message :from from-id :to to-id
                             :operation :test
                             :data '(:key "value"))))
      (multiple-value-bind (env-str pay-str) (apis:serialize-message msg)
        (let ((from-parts (apis:deserialize-message env-str pay-str))
              (from-full (apis:deserialize-message-full
                          (apis:serialize-message-full msg))))
          (check-equal (apis:message-id from-parts)
                       (apis:message-id from-full))
          (check-equal (apis:message-from from-parts)
                       (apis:message-from from-full))
          (check-equal (apis:message-to from-parts)
                       (apis:message-to from-full))
          (check-equal (apis:message-operation from-parts)
                       (apis:message-operation from-full))
          (check-equal (apis:message-data from-parts)
                       (apis:message-data from-full))
          (check-equal (apis:message-timestamp from-parts)
                       (apis:message-timestamp from-full))
          (check-equal (apis:message-time-to-live from-parts)
                       (apis:message-time-to-live from-full))
          (check-equal (apis:message-cause from-parts)
                       (apis:message-cause from-full)))))))

;;; =====================================================================
;;; Message print-object with new address types
;;; =====================================================================

(define-test message-print-with-string-address
  ;; Just verify it doesn't signal an error
  (let* ((msg (apis:message :from "apis://host-a/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                            :to "apis://host-b/01BXQM6K3JANNHPS0BQ0DQZTGV"
                            :operation :test))
         (printed (prin1-to-string msg)))
    (check (stringp printed) "print-object should produce a string")
    (check (search "host-a" printed)
           "printed form should contain the from host")))

(define-test message-print-with-mixed-addresses
  ;; integer from, string to
  (let* ((id (apis:makeid))
         (msg (apis:message :from id
                            :to "apis://remote/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                            :operation :test))
         (printed (prin1-to-string msg)))
    (check (stringp printed) "print-object should handle mixed address types")))
