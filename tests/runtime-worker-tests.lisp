;;;; ***********************************************************************
;;;;
;;;; Name:          runtime-worker-tests.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       tests for Stage 6 runtime-worker
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis-tests)

;;; =====================================================================
;;; Test helper: tcp-test-worker needs to handle :version replies
;;; =====================================================================

(defmethod apis:handle-message ((w tcp-test-worker) msg
                                (op (eql :version)) data)
  (queues:qpush (tcp-test-received w) (list :operation op :data data
                                            :from (apis:message-from msg)
                                            :cause (apis:message-cause msg))))

;;; =====================================================================
;;; Runtime-worker creation and registry
;;; =====================================================================

(define-test runtime-worker-exists-on-default-runtime
  ;; The default runtime should have a runtime-worker installed
  ;; (by the load-time form at the end of runtime-worker.lisp).
  (let ((rw (apis:runtime-worker apis:*default-runtime*)))
    (check (not (null rw))
           "default runtime should have a runtime-worker")
    (check (typep rw 'apis:runtime-worker)
           "runtime-worker should be a runtime-worker instance")))

(define-test runtime-worker-is-registered
  ;; The runtime-worker should be findable in the runtime's registry.
  (let* ((rw (apis:runtime-worker apis:*default-runtime*))
         (found (apis::find-worker (apis:worker-id rw)
                                   apis:*default-runtime*)))
    (check (eq rw found)
           "runtime-worker should be in the registry")))

(define-test runtime-worker-auto-installed-on-new-runtime
  ;; A newly-created runtime should automatically get a runtime-worker.
  (let* ((rt (apis:make-runtime :thread-count 2))
         (rw (apis:runtime-worker rt)))
    (check (not (null rw))
           "new runtime should have a runtime-worker")
    (check (typep rw 'apis:runtime-worker)
           "should be a runtime-worker instance")))

(define-test runtime-worker-has-owning-runtime
  (let* ((rt (apis:make-runtime :thread-count 2))
         (rw (apis:runtime-worker rt)))
    (check (eq rt (apis:runtime-worker-owning-runtime rw))
           "runtime-worker should reference its owning runtime")))

;;; =====================================================================
;;; Swap runtime-worker
;;; =====================================================================

(define-test swap-runtime-worker-replaces
  (let* ((rt (apis:make-runtime :thread-count 2))
         (old-rw (apis:runtime-worker rt))
         (old-id (apis:worker-id old-rw))
         (new-rw (make-instance 'apis:runtime-worker
                                :runtime rt
                                :description "replacement")))
    (apis:swap-runtime-worker rt new-rw)
    (check (eq new-rw (apis:runtime-worker rt))
           "runtime-worker slot should point to the new worker")
    ;; Old worker should be deregistered from rt
    (check (null (apis::find-worker old-id rt))
           "old runtime-worker should be deregistered")
    ;; New worker should be registered in rt
    (check (eq new-rw (apis::find-worker (apis:worker-id new-rw) rt))
           "new runtime-worker should be registered")))

;;; =====================================================================
;;; Pending-reply tracking
;;; =====================================================================

(define-test pending-reply-register-and-count
  (let* ((rt (apis:make-runtime :thread-count 2))
         (rw (apis:runtime-worker rt))
         (msg-id (apis:makeid)))
    (check-equal 0 (apis:pending-reply-count rw)
                 "should start with zero pending replies")
    (apis:register-pending-reply rw msg-id (lambda (m) (declare (ignore m))))
    (check-equal 1 (apis:pending-reply-count rw)
                 "should have one pending reply after register")))

(define-test pending-reply-dispatch-invokes-callback
  (let* ((rt (apis:make-runtime :thread-count 2))
         (rw (apis:runtime-worker rt))
         (msg-id (apis:makeid))
         (received nil))
    (apis:register-pending-reply rw msg-id
                                 (lambda (m)
                                   (setf received (apis:message-data m))))
    ;; Simulate a reply arriving
    (let ((reply (apis:message :operation :version
                               :cause msg-id
                               :data '(:version "0.9.6"))))
      (apis::dispatch-pending-reply rw msg-id reply))
    (check (not (null received))
           "callback should have been invoked")
    (check-equal "0.9.6" (getf received :version)
                 "callback should receive the reply data")
    (check-equal 0 (apis:pending-reply-count rw)
                 "pending reply should be removed after dispatch")))

(define-test pending-reply-dispatch-returns-nil-for-unknown
  (let* ((rt (apis:make-runtime :thread-count 2))
         (rw (apis:runtime-worker rt))
         (unknown-id (apis:makeid))
         (reply (apis:message :operation :version :cause unknown-id)))
    (check (not (apis::dispatch-pending-reply rw unknown-id reply))
           "dispatch should return nil for unknown cause-id")))

(define-test pending-reply-one-shot
  ;; A callback is removed after being invoked once.
  (let* ((rt (apis:make-runtime :thread-count 2))
         (rw (apis:runtime-worker rt))
         (msg-id (apis:makeid))
         (call-count 0))
    (apis:register-pending-reply rw msg-id
                                 (lambda (m)
                                   (declare (ignore m))
                                   (incf call-count)))
    (let ((reply (apis:message :operation :ack :cause msg-id)))
      (apis::dispatch-pending-reply rw msg-id reply)
      ;; Second dispatch should be a no-op
      (apis::dispatch-pending-reply rw msg-id reply))
    (check-equal 1 call-count
                 "callback should only be invoked once")))

;;; =====================================================================
;;; Receive :around intercepts replies
;;; =====================================================================

(define-test runtime-worker-receive-dispatches-reply
  ;; When a message with a matching CAUSE arrives at the runtime-worker,
  ;; the :around method should invoke the callback, not handle-message.
  (let* ((rt (apis:make-runtime :thread-count 2))
         (rw (apis:runtime-worker rt))
         (msg-id (apis:makeid))
         (callback-data nil))
    (apis:register-pending-reply rw msg-id
                                 (lambda (m)
                                   (setf callback-data (apis:message-data m))))
    ;; Deliver a reply directly via receive
    (let ((reply (apis:message :to (apis:worker-id rw)
                               :operation :version
                               :cause msg-id
                               :data '(:version "test"))))
      (apis:receive rw reply))
    (check (not (null callback-data))
           "receive should route reply to callback")
    (check-equal "test" (getf callback-data :version))))

(define-test runtime-worker-receive-falls-through-for-non-reply
  ;; A normal message (no matching pending reply) should go through
  ;; normal handle-message dispatch.  :ping is handled by the default
  ;; method on worker.
  (let* ((rt (apis:make-runtime :thread-count 2))
         (rw (apis:runtime-worker rt)))
    ;; :ping should not error — it's handled by worker's default method
    (let ((msg (apis:message :to (apis:worker-id rw)
                             :operation :ping)))
      ;; This should print the ping message, not error
      (apis:receive rw msg)
      (check t "receive of :ping should not error"))))

;;; =====================================================================
;;; :version handler
;;; =====================================================================

(define-test version-handler-sends-reply
  ;; Send a :version request to the runtime-worker and verify it sends
  ;; a reply with the correct CAUSE and version data.
  (let* ((rt (apis:make-runtime :thread-count 2))
         (old-runtime apis:*default-runtime*)
         (rw (apis:runtime-worker rt)))
    (setf apis:*default-runtime* rt)
    (unwind-protect
         (progn
           (apis:start-runtime rt)
           ;; Create a worker to receive the reply
           (let ((reply-worker (make-instance 'tcp-test-worker)))
             ;; Set up the request message
             (let* ((request-id (apis:makeid))
                    (req (apis:message :id request-id
                                       :from (apis:worker-id reply-worker)
                                       :to (apis:worker-id rw)
                                       :operation :version)))
               ;; Deliver the request to the runtime-worker
               (apis::deliver-locally req (apis:worker-id rw) rt)
               ;; Wait for the reply to arrive at reply-worker
               (let ((result (wait-for-received reply-worker :timeout 5.0)))
                 (check result ":version handler should send a reply")
                 (when result
                   (let ((data (getf result :data)))
                     (check (stringp (getf data :version))
                            "reply should contain a :version string"))
                   (check-equal request-id (getf result :cause)
                                "reply CAUSE should match the request ID"))))))
      (apis:stop-runtime rt)
      (setf apis:*default-runtime* old-runtime))))

;;; =====================================================================
;;; request function
;;; =====================================================================

(define-test request-registers-callback-and-sends
  ;; The request function should register a callback and send the message.
  (let* ((rt (apis:make-runtime :thread-count 2))
         (old-runtime apis:*default-runtime*)
         (rw (apis:runtime-worker rt))
         (callback-invoked nil))
    (setf apis:*default-runtime* rt)
    (unwind-protect
         (progn
           (apis:start-runtime rt)
           ;; Send a request to our own runtime-worker for :version
           (let ((req-id (apis:request
                          (apis:message :to (apis:worker-id rw)
                                        :operation :version)
                          (lambda (reply)
                            (setf callback-invoked
                                  (apis:message-data reply)))
                          :runtime rt)))
             ;; The request should have registered a pending reply
             (check (integerp req-id)
                    "request should return a message ID")
             ;; Wait for the callback to fire (the runtime-worker
             ;; handles the :version, sends a reply back to itself)
             (sleep 1.0)
             (check (not (null callback-invoked))
                    "callback should be invoked with the reply")
             (when callback-invoked
               (check (stringp (getf callback-invoked :version))
                      "reply should contain a version string"))))
      (apis:stop-runtime rt)
      (setf apis:*default-runtime* old-runtime))))

(define-test request-fills-nil-from
  ;; When FROM is nil, request should fill it with the runtime-worker's ID.
  (let* ((rt (apis:make-runtime :thread-count 2))
         (old-runtime apis:*default-runtime*)
         (rw (apis:runtime-worker rt)))
    (setf apis:*default-runtime* rt)
    (unwind-protect
         (progn
           (apis:start-runtime rt)
           ;; Create a receiving worker that records the FROM field
           (let ((receiver (make-instance 'tcp-test-worker)))
             (apis:request
              (apis:message :to (apis:worker-id receiver)
                            :operation :tcp-test
                            :data '(:test "from-fill"))
              (lambda (m) (declare (ignore m)))
              :runtime rt)
             (let ((result (wait-for-received receiver :timeout 5.0)))
               (check result "message should be delivered")
               (when result
                 ;; The FROM should be the runtime-worker's ID
                 ;; (as a local integer, since it's local delivery)
                 (let ((from (getf result :from)))
                   (check (eql from (apis:worker-id rw))
                          "FROM should be the runtime-worker's ID"))))))
      (apis:stop-runtime rt)
      (setf apis:*default-runtime* old-runtime))))

(define-test request-errors-without-runtime-worker
  ;; Calling request on a runtime with no runtime-worker should signal
  ;; a clear error, not a mysterious nil dereference.
  (let* ((rt (make-instance 'apis:runtime :thread-count 2)))
    ;; rt has no runtime-worker (we bypassed make-runtime)
    (check (null (apis:runtime-worker rt))
           "test setup: runtime should have no runtime-worker")
    (check-condition error
      (apis:request (apis:message :to (apis:makeid) :operation :ping)
                    (lambda (m) (declare (ignore m)))
                    :runtime rt))))

;;; =====================================================================
;;; deliver-remotely fills nil FROM from runtime-worker
;;; =====================================================================

(define-test deliver-remotely-fills-nil-from
  ;; When a message with nil FROM is sent remotely, deliver-remotely
  ;; should substitute the runtime-worker's ID.
  (let* ((rt (apis:make-runtime :thread-count 2))
         (old-runtime apis:*default-runtime*)
         (rw (apis:runtime-worker rt))
         (tr (make-instance 'apis:loopback-transport
                            :local-authority "myhost:9100")))
    (setf apis:*default-runtime* rt)
    (unwind-protect
         (progn
           (let ((msg (apis:message
                       :to "apis://remote:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                       :operation :test
                       :data '(:value 1))))
             ;; FROM is nil
             (check (null (apis:message-from msg))
                    "FROM should be nil before delivery")
             (apis::deliver-remotely msg tr rt)
             ;; Receive and check FROM
             (multiple-value-bind (env-str pay-str)
                 (apis:transport-receive tr)
               (let ((restored (apis:deserialize-message env-str pay-str)))
                 (check (stringp (apis:message-from restored))
                        "FROM should be a string after remote delivery")
                 (check (search "myhost:9100" (apis:message-from restored))
                        "FROM should contain local authority")
                 (check (search (apis:format-id (apis:worker-id rw))
                                (apis:message-from restored))
                        "FROM should contain runtime-worker's ULID")))))
      (setf apis:*default-runtime* old-runtime))))

(define-test deliver-remotely-preserves-explicit-from
  ;; When a message has an explicit FROM, deliver-remotely should NOT
  ;; replace it with the runtime-worker's ID.
  (let* ((rt (apis:make-runtime :thread-count 2))
         (old-runtime apis:*default-runtime*)
         (sender-id (apis:makeid))
         (tr (make-instance 'apis:loopback-transport
                            :local-authority "myhost:9100")))
    (setf apis:*default-runtime* rt)
    (unwind-protect
         (progn
           (let ((msg (apis:message
                       :from sender-id
                       :to "apis://remote:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                       :operation :test)))
             (apis::deliver-remotely msg tr rt)
             (multiple-value-bind (env-str pay-str)
                 (apis:transport-receive tr)
               (let ((restored (apis:deserialize-message env-str pay-str)))
                 ;; FROM should contain the original sender's ULID, not
                 ;; the runtime-worker's
                 (check (search (apis:format-id sender-id)
                                (apis:message-from restored))
                        "FROM should contain the original sender's ULID")))))
      (setf apis:*default-runtime* old-runtime))))
