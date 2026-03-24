;;;; ***********************************************************************
;;;;
;;;; Name:          tcp-tests.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       tests for Stage 5 TCP transport
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis-tests)

;;; =====================================================================
;;; Test infrastructure
;;; =====================================================================

(defvar *tcp-test-port-counter* 19100
  "Counter for allocating unique ports across TCP tests.")

(defun next-tcp-test-port ()
  "Return a fresh port number for a test."
  (incf *tcp-test-port-counter*))

;;; A worker subclass that collects received messages for verification.
(defclass tcp-test-worker (apis:worker)
  ((received :accessor tcp-test-received
             :initform (make-instance 'queues:simple-cqueue))))

(defmethod apis:handle-message ((w tcp-test-worker) msg (op (eql :tcp-test)) data)
  (queues:qpush (tcp-test-received w) (list :operation op :data data
                                            :from (apis:message-from msg))))

(defmethod apis:handle-message ((w tcp-test-worker) msg (op (eql :payment)) data)
  (queues:qpush (tcp-test-received w) (list :operation op :data data)))

(defmethod apis:handle-message ((w tcp-test-worker) msg (op (eql :ping)) data)
  (queues:qpush (tcp-test-received w) (list :operation op :data data)))

(defun wait-for-received (worker &key (timeout 5.0) (interval 0.05))
  "Wait up to TIMEOUT seconds for a message on WORKER's received queue.
Returns the message plist or NIL on timeout."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop while (< (get-internal-real-time) deadline)
          when (plusp (queues:qsize (tcp-test-received worker)))
            return (queues:qpop (tcp-test-received worker))
          do (sleep interval)
          finally (return nil))))

;;; =====================================================================
;;; Low-level: TCP wire protocol (no pipeline, no listener)
;;; =====================================================================

(define-test tcp-raw-octets-round-trip
  ;; Two tcp-transports connected via 127.0.0.1, write raw framed
  ;; bytes from one and read from the other.
  (let* ((port (next-tcp-test-port))
         (server-socket (usocket:socket-listen "127.0.0.1" port
                                               :element-type '(unsigned-byte 8)
                                               :reuse-address t))
         (client-socket nil)
         (accepted-socket nil))
    (unwind-protect
         (progn
           (setf client-socket
                 (usocket:socket-connect "127.0.0.1" port
                                         :element-type '(unsigned-byte 8)))
           (setf accepted-socket
                 (usocket:socket-accept server-socket
                                        :element-type '(unsigned-byte 8)))
           (let ((sender (make-instance 'apis:tcp-transport
                                        :authority "127.0.0.1"
                                        :local-authority "127.0.0.1"
                                        :socket client-socket
                                        :stream (usocket:socket-stream client-socket)))
                 (receiver (make-instance 'apis:tcp-transport
                                          :authority "127.0.0.1"
                                          :local-authority "127.0.0.1"
                                          :socket accepted-socket
                                          :stream (usocket:socket-stream accepted-socket))))
             ;; Send some raw bytes
             (let ((payload (apis::string-to-octets "hello over tcp")))
               (apis:transport-write-bytes sender payload)
               (let ((received (apis:transport-read-bytes receiver)))
                 (check (equalp payload received)
                        "raw octets should round-trip over TCP")))))
      ;; Cleanup
      (when client-socket
        (handler-case (usocket:socket-close client-socket) (error () nil)))
      (when accepted-socket
        (handler-case (usocket:socket-close accepted-socket) (error () nil)))
      (handler-case (usocket:socket-close server-socket) (error () nil)))))

;;; =====================================================================
;;; Pipeline over TCP (no listener, manual accept)
;;; =====================================================================

(define-test tcp-pipeline-round-trip
  ;; Use transport-send and transport-receive over TCP — the full
  ;; serialize → transform → frame → write / read → deframe → reverse
  ;; pipeline, without a listener.
  (let* ((port (next-tcp-test-port))
         (server-socket (usocket:socket-listen "127.0.0.1" port
                                               :element-type '(unsigned-byte 8)
                                               :reuse-address t))
         (client-socket nil)
         (accepted-socket nil))
    (unwind-protect
         (progn
           (setf client-socket
                 (usocket:socket-connect "127.0.0.1" port
                                         :element-type '(unsigned-byte 8)))
           (setf accepted-socket
                 (usocket:socket-accept server-socket
                                        :element-type '(unsigned-byte 8)))
           (let ((sender (make-instance 'apis:tcp-transport
                                        :authority (format nil "127.0.0.1:~D" port)
                                        :local-authority "sender:9100"
                                        :socket client-socket
                                        :stream (usocket:socket-stream client-socket)))
                 (receiver (make-instance 'apis:tcp-transport
                                          :authority "incoming"
                                          :local-authority (format nil "127.0.0.1:~D" port)
                                          :socket accepted-socket
                                          :stream (usocket:socket-stream accepted-socket))))
             (multiple-value-bind (id1 id2) (make-test-ids)
               (let* ((msg (apis:message :id id1 :from id2
                                         :to "apis://remote:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                                         :operation :greet
                                         :data '(:name "Alice" :count 42)))
                      (enriched (apis::enrich-from-address msg "sender:9100")))
                 (multiple-value-bind (env-str pay-str)
                     (apis:serialize-message enriched)
                   (apis:transport-send sender env-str pay-str :operation :greet)
                   (multiple-value-bind (env-str2 pay-str2)
                       (apis:transport-receive receiver)
                     (let ((restored (apis:deserialize-message env-str2 pay-str2)))
                       (check-equal (apis:message-id msg) (apis:message-id restored)
                                    "message ID should survive TCP round-trip")
                       (check-equal :greet (apis:message-operation restored)
                                    "operation should survive TCP round-trip")
                       (check-equal "Alice"
                                    (getf (apis:message-data restored) :name)
                                    ":name should survive TCP round-trip")
                       (check-equal 42
                                    (getf (apis:message-data restored) :count)
                                    ":count should survive TCP round-trip"))))))))
      (when client-socket
        (handler-case (usocket:socket-close client-socket) (error () nil)))
      (when accepted-socket
        (handler-case (usocket:socket-close accepted-socket) (error () nil)))
      (handler-case (usocket:socket-close server-socket) (error () nil)))))

(define-test tcp-pipeline-with-encryption
  ;; Full pipeline with sign-then-encrypt over TCP.
  (let* ((port (next-tcp-test-port))
         (enc-key (make-test-key 30))
         (sig-key (make-test-key 40))
         (sign-xf (apis:make-signing-transform sig-key))
         (enc-xf (apis:make-encryption-transform enc-key))
         (transforms (list sign-xf enc-xf))
         (server-socket (usocket:socket-listen "127.0.0.1" port
                                               :element-type '(unsigned-byte 8)
                                               :reuse-address t))
         (client-socket nil)
         (accepted-socket nil))
    (unwind-protect
         (progn
           (setf client-socket
                 (usocket:socket-connect "127.0.0.1" port
                                         :element-type '(unsigned-byte 8)))
           (setf accepted-socket
                 (usocket:socket-accept server-socket
                                        :element-type '(unsigned-byte 8)))
           (let ((sender (make-instance 'apis:tcp-transport
                                        :authority (format nil "127.0.0.1:~D" port)
                                        :local-authority "sender:9100"
                                        :transforms transforms
                                        :socket client-socket
                                        :stream (usocket:socket-stream client-socket)))
                 (receiver (make-instance 'apis:tcp-transport
                                          :authority "incoming"
                                          :local-authority (format nil "127.0.0.1:~D" port)
                                          :transforms transforms
                                          :socket accepted-socket
                                          :stream (usocket:socket-stream accepted-socket))))
             (multiple-value-bind (id1 id2) (make-test-ids)
               (let* ((msg (apis:message :id id1 :from id2
                                         :to "apis://remote:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                                         :operation :payment
                                         :data '(:amount 5000 :currency :eur)))
                      (enriched (apis::enrich-from-address msg "sender:9100")))
                 (multiple-value-bind (env-str pay-str)
                     (apis:serialize-message enriched)
                   (apis:transport-send sender env-str pay-str :operation :payment)
                   (multiple-value-bind (env-str2 pay-str2)
                       (apis:transport-receive receiver)
                     (let ((restored (apis:deserialize-message env-str2 pay-str2)))
                       (check-equal :payment (apis:message-operation restored)
                                    "operation should survive encrypted TCP")
                       (check-equal 5000
                                    (getf (apis:message-data restored) :amount)
                                    ":amount should survive encrypted TCP")
                       (check-equal :eur
                                    (getf (apis:message-data restored) :currency)
                                    ":currency should survive encrypted TCP"))))))))
      (when client-socket
        (handler-case (usocket:socket-close client-socket) (error () nil)))
      (when accepted-socket
        (handler-case (usocket:socket-close accepted-socket) (error () nil)))
      (handler-case (usocket:socket-close server-socket) (error () nil)))))

(define-test tcp-multiple-messages-one-connection
  ;; Send several messages over the same TCP connection.
  (let* ((port (next-tcp-test-port))
         (server-socket (usocket:socket-listen "127.0.0.1" port
                                               :element-type '(unsigned-byte 8)
                                               :reuse-address t))
         (client-socket nil)
         (accepted-socket nil))
    (unwind-protect
         (progn
           (setf client-socket
                 (usocket:socket-connect "127.0.0.1" port
                                         :element-type '(unsigned-byte 8)))
           (setf accepted-socket
                 (usocket:socket-accept server-socket
                                        :element-type '(unsigned-byte 8)))
           (let ((sender (make-instance 'apis:tcp-transport
                                        :authority (format nil "127.0.0.1:~D" port)
                                        :local-authority "sender:9100"
                                        :socket client-socket
                                        :stream (usocket:socket-stream client-socket)))
                 (receiver (make-instance 'apis:tcp-transport
                                          :authority "incoming"
                                          :local-authority (format nil "127.0.0.1:~D" port)
                                          :socket accepted-socket
                                          :stream (usocket:socket-stream accepted-socket))))
             ;; Send 5 messages
             (dotimes (i 5)
               (let* ((msg (apis:message :to "apis://remote:9100/01ARZ3NDEKTSV4RRFFQ69G5FAV"
                                         :operation :greet
                                         :data (list :seq i)))
                      (enriched (apis::enrich-from-address msg "sender:9100")))
                 (multiple-value-bind (env-str pay-str)
                     (apis:serialize-message enriched)
                   (apis:transport-send sender env-str pay-str :operation :greet))))
             ;; Receive 5 messages and verify order
             (dotimes (i 5)
               (multiple-value-bind (env-str pay-str)
                   (apis:transport-receive receiver)
                 (let ((restored (apis:deserialize-message env-str pay-str)))
                   (check-equal i (getf (apis:message-data restored) :seq)
                                (format nil "message ~D should arrive in order" i)))))))
      (when client-socket
        (handler-case (usocket:socket-close client-socket) (error () nil)))
      (when accepted-socket
        (handler-case (usocket:socket-close accepted-socket) (error () nil)))
      (handler-case (usocket:socket-close server-socket) (error () nil)))))

;;; =====================================================================
;;; connect-tcp convenience function
;;; =====================================================================

(define-test connect-tcp-creates-transport
  (let* ((port (next-tcp-test-port))
         (server-socket (usocket:socket-listen "127.0.0.1" port
                                               :element-type '(unsigned-byte 8)
                                               :reuse-address t))
         (tr nil))
    (unwind-protect
         (progn
           (setf tr (apis:connect-tcp "127.0.0.1" port
                                      :local-authority "myhost:9100"))
           (check (typep tr 'apis:tcp-transport)
                  "connect-tcp should return a tcp-transport")
           (check-equal (format nil "127.0.0.1:~D" port)
                        (apis:transport-authority tr)
                        "authority should be host:port")
           (check-equal "myhost:9100"
                        (apis:transport-local-authority tr)
                        "local-authority should be passed through"))
      (when tr (handler-case (apis:transport-close tr) (error () nil)))
      (handler-case (usocket:socket-close server-socket) (error () nil)))))

;;; =====================================================================
;;; tcp-listener: full end-to-end delivery
;;; =====================================================================

(define-test tcp-listener-delivers-message
  ;; Start a runtime with a test worker, start a listener, connect
  ;; from a client, send a message, and verify the worker receives it.
  (let* ((port (next-tcp-test-port))
         (rt (apis:make-runtime :thread-count 2))
         (old-runtime apis:*default-runtime*)
         (listener nil)
         (client nil))
    (setf apis:*default-runtime* rt)
    (unwind-protect
         (let ((worker (make-instance 'tcp-test-worker)))
           (apis:start-runtime rt)
           (setf listener
                 (make-instance 'apis:tcp-listener
                                :host "127.0.0.1"
                                :port port
                                :runtime rt))
           (apis:start-tcp-listener listener)
           ;; Give the listener a moment to bind
           (sleep 0.1)
           ;; Connect and send
           (setf client (apis:connect-tcp "127.0.0.1" port
                                          :local-authority "sender:9100"))
           (let* ((to-addr (format nil "apis://127.0.0.1:~D/~A"
                                   port (apis:format-id (apis:worker-id worker))))
                  (msg (apis:message :to to-addr
                                     :operation :tcp-test
                                     :data '(:greeting "hello via tcp")))
                  (enriched (apis::enrich-from-address msg "sender:9100")))
             (multiple-value-bind (env-str pay-str)
                 (apis:serialize-message enriched)
               (apis:transport-send client env-str pay-str :operation :tcp-test))
             ;; Wait for delivery
             (let ((result (wait-for-received worker :timeout 5.0)))
               (check result "worker should receive the message")
               (when result
                 (check-equal :tcp-test (getf result :operation)
                              "operation should be :tcp-test")
                 (check-equal "hello via tcp"
                              (getf (getf result :data) :greeting)
                              ":greeting should arrive intact")))))
      ;; Cleanup
      (when client (handler-case (apis:transport-close client) (error () nil)))
      (when listener (apis:stop-tcp-listener listener))
      (apis:stop-runtime rt)
      (setf apis:*default-runtime* old-runtime))))

(define-test tcp-listener-preserves-message-fields
  ;; Verify that all message fields survive the TCP journey, including
  ;; FROM address enrichment.
  (let* ((port (next-tcp-test-port))
         (rt (apis:make-runtime :thread-count 2))
         (old-runtime apis:*default-runtime*)
         (listener nil)
         (client nil))
    (setf apis:*default-runtime* rt)
    (unwind-protect
         (let ((worker (make-instance 'tcp-test-worker))
               (sender-id (apis:makeid)))
           (apis:start-runtime rt)
           (setf listener
                 (make-instance 'apis:tcp-listener
                                :host "127.0.0.1"
                                :port port
                                :runtime rt))
           (apis:start-tcp-listener listener)
           (sleep 0.1)
           (setf client (apis:connect-tcp "127.0.0.1" port
                                          :local-authority "sender:9100"))
           (let* ((to-addr (format nil "apis://127.0.0.1:~D/~A"
                                   port (apis:format-id (apis:worker-id worker))))
                  (msg (apis:message :from sender-id
                                     :to to-addr
                                     :operation :tcp-test
                                     :data '(:key1 "value1" :key2 42 :key3 3.14d0)))
                  (enriched (apis::enrich-from-address msg "sender:9100")))
             (multiple-value-bind (env-str pay-str)
                 (apis:serialize-message enriched)
               (apis:transport-send client env-str pay-str :operation :tcp-test))
             (let ((result (wait-for-received worker :timeout 5.0)))
               (check result "worker should receive the message")
               (when result
                 (let ((data (getf result :data)))
                   (check-equal "value1" (getf data :key1)
                                "string payload should survive")
                   (check-equal 42 (getf data :key2)
                                "integer payload should survive")
                   (check-equal 3.14d0 (getf data :key3)
                                "float payload should survive"))
                 ;; FROM should be enriched to a URI containing sender:9100
                 (let ((from (getf result :from)))
                   (check (stringp from)
                          "FROM should be a string after TCP delivery")
                   (check (search "sender:9100" from)
                          "FROM should contain the sender's authority"))))))
      (when client (handler-case (apis:transport-close client) (error () nil)))
      (when listener (apis:stop-tcp-listener listener))
      (apis:stop-runtime rt)
      (setf apis:*default-runtime* old-runtime))))

(define-test tcp-listener-with-encryption
  ;; End-to-end with sign-then-encrypt transforms.
  (let* ((port (next-tcp-test-port))
         (enc-key (make-test-key 50))
         (sig-key (make-test-key 60))
         (sign-xf (apis:make-signing-transform sig-key))
         (enc-xf (apis:make-encryption-transform enc-key))
         (transforms (list sign-xf enc-xf))
         (rt (apis:make-runtime :thread-count 2))
         (old-runtime apis:*default-runtime*)
         (listener nil)
         (client nil))
    (setf apis:*default-runtime* rt)
    (unwind-protect
         (let ((worker (make-instance 'tcp-test-worker)))
           (apis:start-runtime rt)
           (setf listener
                 (make-instance 'apis:tcp-listener
                                :host "127.0.0.1"
                                :port port
                                :transforms transforms
                                :runtime rt))
           (apis:start-tcp-listener listener)
           (sleep 0.1)
           (setf client (apis:connect-tcp "127.0.0.1" port
                                          :transforms transforms
                                          :local-authority "sender:9100"))
           (let* ((to-addr (format nil "apis://127.0.0.1:~D/~A"
                                   port (apis:format-id (apis:worker-id worker))))
                  (msg (apis:message :to to-addr
                                     :operation :tcp-test
                                     :data '(:secret "encrypted-payload")))
                  (enriched (apis::enrich-from-address msg "sender:9100")))
             (multiple-value-bind (env-str pay-str)
                 (apis:serialize-message enriched)
               (apis:transport-send client env-str pay-str :operation :tcp-test))
             (let ((result (wait-for-received worker :timeout 5.0)))
               (check result "worker should receive encrypted message")
               (when result
                 (check-equal "encrypted-payload"
                              (getf (getf result :data) :secret)
                              "encrypted payload should round-trip")))))
      (when client (handler-case (apis:transport-close client) (error () nil)))
      (when listener (apis:stop-tcp-listener listener))
      (apis:stop-runtime rt)
      (setf apis:*default-runtime* old-runtime))))

(define-test tcp-listener-per-operation-policy
  ;; Per-operation transform policy: :payment encrypted, :ping signed only.
  (let* ((port (next-tcp-test-port))
         (enc-key (make-test-key 70))
         (sig-key (make-test-key 80))
         (sign-xf (apis:make-signing-transform sig-key))
         (enc-xf (apis:make-encryption-transform enc-key))
         (policy (lambda (op)
                   (case op
                     (:payment (list sign-xf enc-xf))
                     (otherwise (list sign-xf)))))
         (rt (apis:make-runtime :thread-count 2))
         (old-runtime apis:*default-runtime*)
         (listener nil)
         (client nil))
    (setf apis:*default-runtime* rt)
    (unwind-protect
         (let ((worker (make-instance 'tcp-test-worker)))
           (apis:start-runtime rt)
           (setf listener
                 (make-instance 'apis:tcp-listener
                                :host "127.0.0.1"
                                :port port
                                :transforms policy
                                :runtime rt))
           (apis:start-tcp-listener listener)
           (sleep 0.1)
           (setf client (apis:connect-tcp "127.0.0.1" port
                                          :transforms policy
                                          :local-authority "sender:9100"))
           (let ((to-addr (format nil "apis://127.0.0.1:~D/~A"
                                  port (apis:format-id (apis:worker-id worker)))))
             ;; Send a :payment (encrypted)
             (let* ((msg (apis:message :to to-addr :operation :payment
                                       :data '(:amount 9999)))
                    (enriched (apis::enrich-from-address msg "sender:9100")))
               (multiple-value-bind (env-str pay-str)
                   (apis:serialize-message enriched)
                 (apis:transport-send client env-str pay-str :operation :payment)))
             (let ((result (wait-for-received worker :timeout 5.0)))
               (check result ":payment should be delivered")
               (when result
                 (check-equal :payment (getf result :operation))
                 (check-equal 9999 (getf (getf result :data) :amount))))
             ;; Send a :ping (signed only)
             (let* ((msg (apis:message :to to-addr :operation :ping
                                       :data '(:seq 1)))
                    (enriched (apis::enrich-from-address msg "sender:9100")))
               (multiple-value-bind (env-str pay-str)
                   (apis:serialize-message enriched)
                 (apis:transport-send client env-str pay-str :operation :ping)))
             (let ((result (wait-for-received worker :timeout 5.0)))
               (check result ":ping should be delivered")
               (when result
                 (check-equal :ping (getf result :operation))
                 (check-equal 1 (getf (getf result :data) :seq))))))
      (when client (handler-case (apis:transport-close client) (error () nil)))
      (when listener (apis:stop-tcp-listener listener))
      (apis:stop-runtime rt)
      (setf apis:*default-runtime* old-runtime))))

(define-test tcp-listener-multiple-messages
  ;; Send multiple messages through the listener and verify all arrive.
  (let* ((port (next-tcp-test-port))
         (rt (apis:make-runtime :thread-count 2))
         (old-runtime apis:*default-runtime*)
         (listener nil)
         (client nil))
    (setf apis:*default-runtime* rt)
    (unwind-protect
         (let ((worker (make-instance 'tcp-test-worker)))
           (apis:start-runtime rt)
           (setf listener
                 (make-instance 'apis:tcp-listener
                                :host "127.0.0.1"
                                :port port
                                :runtime rt))
           (apis:start-tcp-listener listener)
           (sleep 0.1)
           (setf client (apis:connect-tcp "127.0.0.1" port
                                          :local-authority "sender:9100"))
           (let ((to-addr (format nil "apis://127.0.0.1:~D/~A"
                                  port (apis:format-id (apis:worker-id worker)))))
             ;; Send 5 messages
             (dotimes (i 5)
               (let* ((msg (apis:message :to to-addr :operation :tcp-test
                                         :data (list :seq i)))
                      (enriched (apis::enrich-from-address msg "sender:9100")))
                 (multiple-value-bind (env-str pay-str)
                     (apis:serialize-message enriched)
                   (apis:transport-send client env-str pay-str
                                        :operation :tcp-test))))
             ;; Collect all 5
             (let ((results nil))
               (dotimes (i 5)
                 (let ((r (wait-for-received worker :timeout 5.0)))
                   (when r (push r results))))
               (check-equal 5 (length results)
                            "all 5 messages should arrive")
               ;; Verify we got all sequence numbers (order may vary
               ;; due to thread scheduling)
               (let ((seqs (sort (mapcar (lambda (r)
                                           (getf (getf r :data) :seq))
                                         results)
                                 #'<)))
                 (check-equal '(0 1 2 3 4) seqs
                              "all sequence numbers should be present")))))
      (when client (handler-case (apis:transport-close client) (error () nil)))
      (when listener (apis:stop-tcp-listener listener))
      (apis:stop-runtime rt)
      (setf apis:*default-runtime* old-runtime))))

(define-test tcp-listener-stop-is-clean
  ;; Starting and stopping a listener should not leak resources or error.
  (let* ((port (next-tcp-test-port))
         (rt (apis:make-runtime :thread-count 2))
         (old-runtime apis:*default-runtime*)
         (listener nil))
    (setf apis:*default-runtime* rt)
    (unwind-protect
         (progn
           (setf listener
                 (make-instance 'apis:tcp-listener
                                :host "127.0.0.1"
                                :port port
                                :runtime rt))
           (apis:start-tcp-listener listener)
           (check (apis:listener-running-p listener)
                  "listener should be running after start")
           (sleep 0.2)
           (apis:stop-tcp-listener listener)
           (check (not (apis:listener-running-p listener))
                  "listener should not be running after stop"))
      (when (and listener (apis:listener-running-p listener))
        (apis:stop-tcp-listener listener))
      (setf apis:*default-runtime* old-runtime))))

(define-test tcp-transport-close-is-idempotent
  ;; Closing a tcp-transport twice should not error.
  (let* ((port (next-tcp-test-port))
         (server-socket (usocket:socket-listen "127.0.0.1" port
                                               :element-type '(unsigned-byte 8)
                                               :reuse-address t))
         (tr nil))
    (unwind-protect
         (progn
           (setf tr (apis:connect-tcp "127.0.0.1" port
                                      :local-authority "test"))
           (apis:transport-close tr)
           ;; Second close should be safe
           (apis:transport-close tr)
           (check t "double close should not error"))
      (handler-case (usocket:socket-close server-socket) (error () nil)))))


;;; =====================================================================
;;; Cross-host test helpers
;;; =====================================================================
;;; These functions are not tests themselves. They provide a simple
;;; setup for running tests between two different machines.
;;;
;;; Step 0 — verify connectivity:
;;;   ;; On either machine:
;;;   (apis-tests:test-tcp-connectivity "other-host" 9100)
;;;
;;; Step 1 — start the receiver:
;;;   ;; On machine A:
;;;   (apis-tests:setup-test-receiver :port 9100)
;;;   ;; → prints Worker ULID
;;;
;;; Step 2 — send from the other machine:
;;;   ;; On machine B:
;;;   (apis-tests:send-test-message-tcp "machine-a-ip" 9100
;;;     "WORKER-ULID-STRING" '(:greeting "hello from B"))
;;;
;;; Step 3 — check on machine A:
;;;   (queues:qpop (apis-tests::tcp-test-received
;;;                  apis-tests:*cross-host-worker*))

(defvar *cross-host-worker* nil
  "The test worker created by SETUP-TEST-RECEIVER, for inspection.")

(defun test-tcp-connectivity (host port &key (timeout 5))
  "Test basic TCP connectivity to HOST:PORT.  Attempts to connect,
reports success or failure with details.  Does not require a listener
running Apis — any TCP listener on that port will do.  Returns T on
success, NIL on failure."
  (format t "~&Testing TCP connection to ~A:~D ...~%" host port)
  (handler-case
      (let ((socket (usocket:socket-connect
                     host port
                     :element-type '(unsigned-byte 8)
                     :timeout timeout)))
        (usocket:socket-close socket)
        (format t "  SUCCESS: Connected and closed cleanly.~%")
        t)
    (usocket:connection-refused-error ()
      (format t "  FAILED: Connection refused. Is anything listening on port ~D?~%" port)
      nil)
    (usocket:timeout-error ()
      (format t "  FAILED: Connection timed out after ~D seconds.~%~
  Likely causes: firewall blocking port ~D, or host ~A unreachable.~%"
              timeout port host)
      nil)
    (usocket:host-unreachable-error ()
      (format t "  FAILED: Host ~A is unreachable.~%" host)
      nil)
    (usocket:socket-error (e)
      (format t "  FAILED: Socket error: ~A~%" e)
      nil)
    (error (e)
      (format t "  FAILED: ~A~%" e)
      nil)))

(defun setup-test-receiver (&key (port 9100) (host "0.0.0.0")
                              transforms)
  "Start a runtime, create a tcp-test-worker, and start a listener.
Prints the worker's ULID so the sender can address it.
Returns (values listener worker runtime old-default-runtime).
Call TEARDOWN-TEST-RECEIVER to clean up."
  (let* ((rt (apis:make-runtime :thread-count 4))
         (old-default apis:*default-runtime*))
    (setf apis:*default-runtime* rt)
    (let ((worker (make-instance 'tcp-test-worker)))
      (setf *cross-host-worker* worker)
      (apis:start-runtime rt)
      (let ((listener (make-instance 'apis:tcp-listener
                                     :host host
                                     :port port
                                     :transforms transforms
                                     :runtime rt)))
        (apis:start-tcp-listener listener)
        (format t "~&Receiver ready on ~A:~D~%" host port)
        (format t "Worker ULID: ~A~%" (apis:format-id (apis:worker-id worker)))
        (format t "~%To verify connectivity from the sender, run:~%")
        (format t "  (apis-tests:test-tcp-connectivity ~S ~D)~%" "<receiver-ip>" port)
        (format t "~%To send a test message:~%")
        (format t "  (apis-tests:send-test-message-tcp ~S ~D ~S~%    '(:greeting ~S))~%"
                "<receiver-ip>" port
                (apis:format-id (apis:worker-id worker))
                "hello")
        (format t "~%Call (apis-tests:teardown-test-receiver ...) to stop.~%")
        (values listener worker rt old-default)))))

(defun teardown-test-receiver (listener runtime
                               &optional old-default-runtime)
  "Stop the listener and runtime created by SETUP-TEST-RECEIVER."
  (apis:stop-tcp-listener listener)
  (apis:stop-runtime runtime)
  (when old-default-runtime
    (setf apis:*default-runtime* old-default-runtime))
  (setf *cross-host-worker* nil)
  (format t "~&Receiver torn down.~%"))

(defun send-test-message-tcp (host port worker-id-string data
                              &key transforms
                                (local-authority "test-sender:9100")
                                (operation :tcp-test))
  "Connect to HOST:PORT, send a single test message to the worker
identified by WORKER-ID-STRING, and close the connection.
DATA is the message payload plist.  TRANSFORMS must match the
receiver's configuration.  Reports errors clearly."
  (format t "~&Connecting to ~A:~D ...~%" host port)
  (let ((tr nil))
    (handler-case
        (progn
          (setf tr (apis:connect-tcp host port
                                     :transforms transforms
                                     :local-authority local-authority))
          (format t "Connected.  Sending ~S ...~%" operation)
          (let* ((to-addr (format nil "apis://~A:~D/~A" host port worker-id-string))
                 (msg (apis:message :to to-addr
                                    :operation operation
                                    :data data))
                 (enriched (apis::enrich-from-address msg local-authority)))
            (multiple-value-bind (env-str pay-str)
                (apis:serialize-message enriched)
              (apis:transport-send tr env-str pay-str :operation operation))
            (format t "Sent ~S to ~A~%" operation to-addr)
            ;; Brief pause to let the data flush before closing
            (sleep 0.1)
            (apis:transport-close tr)
            (setf tr nil)
            (format t "Connection closed.  Check the receiver.~%")
            t))
      (usocket:connection-refused-error ()
        (format t "FAILED: Connection refused. Is the listener running on ~A:~D?~%"
                host port)
        nil)
      (usocket:timeout-error ()
        (format t "FAILED: Connection timed out. Firewall or host unreachable?~%")
        nil)
      (error (e)
        (format t "FAILED: ~A~%" e)
        (when tr
          (handler-case (apis:transport-close tr) (error () nil)))
        nil))))
