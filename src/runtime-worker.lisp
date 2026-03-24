;;;; ***********************************************************************
;;;;
;;;; Name:          runtime-worker.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       runtime-worker: meta-protocol handler and request
;;;;                tracking for runtimes
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; CLASS runtime-worker
;;; ---------------------------------------------------------------------
;;; A runtime-worker is a regular worker owned by a runtime. It serves
;;; two purposes:
;;;
;;; 1. It handles meta-protocol operations (:ping, :version) on behalf
;;;    of the runtime.  When a remote runtime wants to query this
;;;    runtime, it addresses messages to the runtime-worker.
;;;
;;; 2. It tracks pending request-reply callbacks.  When code on this
;;;    runtime sends a request and expects a reply, the callback is
;;;    registered here.  When a reply arrives (identified by the CAUSE
;;;    field matching a pending message ID), the callback is invoked
;;;    instead of normal handle-message dispatch.
;;;
;;; The runtime-worker is a pet worker: it can be swapped out at any
;;; time for a new instance with different behavior.

(defclass runtime-worker (worker)
  ((owning-runtime :reader runtime-worker-owning-runtime
                   :initarg :runtime
                   :documentation "The runtime that owns this worker.")
   (pending-replies :reader runtime-worker-pending-replies
                    :initform (make-hash-table :test 'eql)
                    :documentation "Hash table mapping message-id (integer) to
callback function.  When a reply arrives whose CAUSE matches a
pending ID, the callback is invoked with the reply message.")
   (pending-lock :reader runtime-worker-pending-lock
                 :initform (bt:make-lock "pending-replies-lock")
                 :documentation "Protects the pending-replies hash table."))
  (:documentation "A worker that handles meta-protocol operations for a
runtime and tracks pending request-reply callbacks."))

(defmethod print-object ((rw runtime-worker) out-stream)
  (print-unreadable-object (rw out-stream :type t :identity nil)
    (format out-stream "~A [~(~A~)]"
            (format-id (worker-id rw))
            (worker-state rw))))

;;; ---------------------------------------------------------------------
;;; Pending-reply tracking
;;; ---------------------------------------------------------------------

(defun register-pending-reply (runtime-worker message-id callback)
  "Register CALLBACK to be called when a reply to MESSAGE-ID arrives.
CALLBACK is a function of one argument: the reply message.
Thread-safe."
  (bt:with-lock-held ((runtime-worker-pending-lock runtime-worker))
    (setf (gethash message-id
                   (runtime-worker-pending-replies runtime-worker))
          callback)))

(defun dispatch-pending-reply (runtime-worker cause-id message)
  "If CAUSE-ID matches a pending reply, invoke the callback with MESSAGE
and remove it from the table.  Returns T if a callback was invoked, NIL
otherwise.  Thread-safe."
  (let ((callback nil))
    (bt:with-lock-held ((runtime-worker-pending-lock runtime-worker))
      (setf callback (gethash cause-id
                              (runtime-worker-pending-replies runtime-worker)))
      (when callback
        (remhash cause-id
                 (runtime-worker-pending-replies runtime-worker))))
    (when callback
      (funcall callback message)
      t)))

(defun pending-reply-count (runtime-worker)
  "Return the number of pending reply callbacks.  Thread-safe."
  (bt:with-lock-held ((runtime-worker-pending-lock runtime-worker))
    (hash-table-count (runtime-worker-pending-replies runtime-worker))))

;;; ---------------------------------------------------------------------
;;; Receive: check pending replies before normal dispatch
;;; ---------------------------------------------------------------------

(defmethod receive :around ((worker runtime-worker) (msg message))
  "Before normal dispatch, check if this message is a reply to a
pending request (identified by the CAUSE field).  If so, invoke
the registered callback instead of handle-message."
  (let ((cause (message-cause msg)))
    (if (and cause (dispatch-pending-reply worker cause msg))
        nil  ; reply handled by callback
        (call-next-method))))

;;; ---------------------------------------------------------------------
;;; Meta-protocol: :version
;;; ---------------------------------------------------------------------

(defun apis-version ()
  "Return the Apis system version string."
  (asdf:component-version (asdf:find-system :apis)))

(defmethod handle-message ((worker runtime-worker) msg
                           (op (eql :version)) data)
  "Handle a :version request by sending a reply with the Apis version
and the runtime's local authority.  The reply's CAUSE is set to the
requesting message's ID so the requester can correlate it."
  (let ((from (message-from msg)))
    (when from
      (send (message :to from
                     :operation :version
                     :cause (message-id msg)
                     :data (list :version (apis-version)
                                 :authority
                                 (runtime-local-authority
                                  (runtime-worker-owning-runtime worker))))))))

;;; ---------------------------------------------------------------------
;;; request: send a message and register a reply callback
;;; ---------------------------------------------------------------------

(defun request (message callback &key (runtime *default-runtime*))
  "Send MESSAGE as a request and register CALLBACK for the reply.
CALLBACK is a function of one argument: the reply message.  It will
be called on a scheduling thread when a message arrives at the
runtime-worker whose CAUSE field matches MESSAGE's ID.

If MESSAGE has a nil FROM field, it is set to the runtime-worker's
ID so that replies are routed back to the runtime-worker.

Returns the message ID (which will appear as CAUSE in the reply)."
  (let* ((rw (runtime-worker runtime))
         (msg (if (null (message-from message))
                  (message :id (message-id message)
                           :from (worker-id rw)
                           :to (message-to message)
                           :operation (message-operation message)
                           :data (message-data message)
                           :timestamp (message-timestamp message)
                           :time-to-live (message-time-to-live message)
                           :cause (message-cause message))
                  message)))
    (register-pending-reply rw (message-id msg) callback)
    (send msg)
    (message-id msg)))

;;; ---------------------------------------------------------------------
;;; install-runtime-worker
;;; ---------------------------------------------------------------------

(defun install-runtime-worker (runtime)
  "Create a runtime-worker for RUNTIME, register it, and store it
in the runtime's runtime-worker slot.  Returns the new runtime-worker.
If the runtime already has a runtime-worker, replaces it."
  (let ((old (runtime-worker runtime)))
    ;; Deregister old worker if present
    (when old
      (bt:with-lock-held ((runtime-registry-lock runtime))
        (remhash (worker-id old) (runtime-registry runtime)))))
  (let ((rw (make-instance 'runtime-worker
                           :runtime runtime
                           :description "runtime-worker")))
    ;; Register in the runtime's own registry
    (bt:with-lock-held ((runtime-registry-lock runtime))
      (setf (gethash (worker-id rw) (runtime-registry runtime)) rw))
    (setf (runtime-worker runtime) rw)
    rw))

;;; ---------------------------------------------------------------------
;;; (setf runtime-worker): swap the runtime-worker
;;; ---------------------------------------------------------------------

(defun swap-runtime-worker (runtime new-worker)
  "Replace the runtime's runtime-worker with NEW-WORKER.
Deregisters the old worker and registers the new one.  Returns
NEW-WORKER."
  (let ((old (runtime-worker runtime)))
    (when old
      (bt:with-lock-held ((runtime-registry-lock runtime))
        (remhash (worker-id old) (runtime-registry runtime)))))
  (bt:with-lock-held ((runtime-registry-lock runtime))
    (setf (gethash (worker-id new-worker) (runtime-registry runtime))
          new-worker))
  (setf (runtime-worker runtime) new-worker)
  new-worker)

;;; ---------------------------------------------------------------------
;;; Auto-install on runtime creation
;;; ---------------------------------------------------------------------
;;; This :after method runs for all runtimes created after this file
;;; is loaded.  The *default-runtime* (created earlier, in runtime.lisp)
;;; is handled below at load time.

(defmethod initialize-instance :after ((runtime runtime) &key)
  (install-runtime-worker runtime))

;;; Ensure the pre-existing *default-runtime* gets a runtime-worker.
(when (and *default-runtime* (null (runtime-worker *default-runtime*)))
  (install-runtime-worker *default-runtime*))
