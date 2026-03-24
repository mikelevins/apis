;;;; ***********************************************************************
;;;;
;;;; Name:          tcp-transport.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       TCP transport and listener
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; TCP wire protocol
;;; ---------------------------------------------------------------------
;;; Each framed message on the TCP stream is preceded by a 4-byte
;;; big-endian length prefix.  This tells the receiver how many bytes
;;; to read for the next complete frame.
;;;
;;;   [4 bytes: frame-length N, big-endian uint32]
;;;   [N bytes: framed message (as produced by frame-message)]
;;;
;;; The framed message has its own internal structure (envelope and
;;; payload with their own length prefixes — see frame-message), but
;;; the TCP layer treats it as opaque bytes.

(defun write-uint32-to-stream (stream n)
  "Write N as a 4-byte big-endian uint32 to STREAM."
  (write-byte (ldb (byte 8 24) n) stream)
  (write-byte (ldb (byte 8 16) n) stream)
  (write-byte (ldb (byte 8  8) n) stream)
  (write-byte (ldb (byte 8  0) n) stream))

(defun read-uint32-from-stream (stream)
  "Read a 4-byte big-endian uint32 from STREAM.
Signals END-OF-FILE if the stream is at end before all four bytes
are read."
  (let ((b0 (read-byte stream))    ; signals end-of-file on EOF
        (b1 (read-byte stream))
        (b2 (read-byte stream))
        (b3 (read-byte stream)))
    (+ (ash b0 24) (ash b1 16) (ash b2 8) b3)))

(defun read-exact-octets (stream count)
  "Read exactly COUNT bytes from STREAM, returning an octet vector.
Signals TRANSPORT-ERROR if the connection closes before all bytes
arrive."
  (if (zerop count)
      (make-array 0 :element-type '(unsigned-byte 8))
      (let ((buf (make-array count :element-type '(unsigned-byte 8)))
            (pos 0))
        (loop while (< pos count)
              do (let ((end (read-sequence buf stream :start pos :end count)))
                   (when (= end pos)
                     (error 'transport-error
                            :reason "Connection closed before all bytes were read"))
                   (setf pos end)))
        buf)))

;;; ---------------------------------------------------------------------
;;; CLASS tcp-transport
;;; ---------------------------------------------------------------------
;;; A transport subclass for TCP connections.  Implements the raw I/O
;;; methods transport-write-bytes and transport-read-bytes; the base
;;; class methods transport-send and transport-receive handle the
;;; serialize → transform → frame pipeline.
;;;
;;; A tcp-transport can be created either by connecting to a remote
;;; host (via connect-tcp) or by wrapping an accepted socket (via the
;;; tcp-listener's accept loop).

(defclass tcp-transport (transport)
  ((socket :accessor tcp-transport-socket :initarg :socket
           :documentation "The usocket socket object.")
   (stream :accessor tcp-transport-stream :initarg :stream
           :documentation "The binary stream for reading/writing octets."))
  (:documentation "A transport that sends and receives framed messages
over a TCP connection.  The wire protocol prepends a 4-byte length
prefix to each framed message so the receiver can delimit messages
on the TCP byte stream."))

(defmethod print-object ((tr tcp-transport) out-stream)
  (print-unreadable-object (tr out-stream :type t :identity nil)
    (format out-stream "~A" (transport-authority tr))))

(defmethod transport-write-bytes ((tr tcp-transport) octets)
  "Write a length-prefixed framed message to the TCP stream."
  (let ((stream (tcp-transport-stream tr)))
    (write-uint32-to-stream stream (length octets))
    (write-sequence octets stream)
    (force-output stream)))

(defmethod transport-read-bytes ((tr tcp-transport))
  "Read a length-prefixed framed message from the TCP stream."
  (let* ((stream (tcp-transport-stream tr))
         (len (read-uint32-from-stream stream)))
    (read-exact-octets stream len)))

(defmethod transport-close ((tr tcp-transport))
  "Close the TCP socket and its stream."
  (handler-case
      (usocket:socket-close (tcp-transport-socket tr))
    (error () nil)))  ; ignore errors on close

;;; ---------------------------------------------------------------------
;;; connect-tcp
;;; ---------------------------------------------------------------------

(defun connect-tcp (host port &key transforms local-authority)
  "Connect to HOST:PORT via TCP and return a tcp-transport.
TRANSFORMS is the transform policy (list or function) for the
pipeline.  LOCAL-AUTHORITY is this runtime's host:port string,
used for FROM-address enrichment."
  (let* ((socket (usocket:socket-connect host port
                                         :element-type '(unsigned-byte 8)))
         (stream (usocket:socket-stream socket))
         (authority (format nil "~A:~D" host port)))
    (make-instance 'tcp-transport
                   :authority authority
                   :local-authority local-authority
                   :transforms transforms
                   :socket socket
                   :stream stream)))

;;; ---------------------------------------------------------------------
;;; CLASS tcp-listener
;;; ---------------------------------------------------------------------
;;; A TCP server that accepts connections and delivers received
;;; messages into a local runtime.  Each accepted connection gets its
;;; own receive-loop thread that reads framed messages, reverses the
;;; transform pipeline, deserializes, and calls deliver-locally.
;;;
;;; Usage:
;;;   (defvar *listener*
;;;     (make-instance 'tcp-listener
;;;       :port 9100
;;;       :transforms (list sign-xf encrypt-xf)
;;;       :runtime *default-runtime*))
;;;   (start-tcp-listener *listener*)
;;;   ;; ... later ...
;;;   (stop-tcp-listener *listener*)

(defclass tcp-listener ()
  ((listen-socket :accessor listener-listen-socket :initform nil
                  :documentation "The usocket server socket.")
   (host :reader listener-host :initarg :host :initform "0.0.0.0"
         :documentation "The interface to listen on.")
   (port :reader listener-port :initarg :port :type integer
         :documentation "The port to listen on.")
   (transforms :reader listener-transforms :initarg :transforms
               :initform nil
               :documentation "Transform policy for accepted connections.
Same format as the transport transforms slot: a flat list or a
function (operation-keyword → transform-list).")
   (runtime :reader listener-runtime :initarg :runtime
            :documentation "The runtime to deliver received messages into.")
   (threads :accessor listener-threads :initform nil
            :documentation "All threads spawned by this listener.")
   (connections :accessor listener-connections :initform nil
                :documentation "All tcp-transport objects for accepted connections.")
   (lock :reader listener-lock
         :initform (bt:make-lock "tcp-listener-lock")
         :documentation "Protects threads and connections lists.")
   (running-p :accessor listener-running-p :initform nil
              :documentation "T while the listener is accepting connections."))
  (:documentation "A TCP server that accepts connections and delivers
received messages into a local runtime."))

(defmethod print-object ((listener tcp-listener) out-stream)
  (print-unreadable-object (listener out-stream :type t :identity nil)
    (format out-stream "~A:~D ~A"
            (listener-host listener)
            (listener-port listener)
            (if (listener-running-p listener) "[listening]" "[stopped]"))))

;;; ---------------------------------------------------------------------
;;; Listener lifecycle
;;; ---------------------------------------------------------------------

(defun start-tcp-listener (listener)
  "Start LISTENER: bind the server socket and spawn the accept loop.
Returns LISTENER."
  (when (listener-running-p listener)
    (return-from start-tcp-listener listener))
  (setf (listener-listen-socket listener)
        (usocket:socket-listen (listener-host listener)
                               (listener-port listener)
                               :element-type '(unsigned-byte 8)
                               :reuse-address t
                               :backlog 5))
  (setf (listener-running-p listener) t)
  (let ((accept-thread
          (bt:make-thread
           (lambda () (tcp-accept-loop listener))
           :name (format nil "apis tcp-accept ~A:~D"
                         (listener-host listener)
                         (listener-port listener)))))
    (bt:with-lock-held ((listener-lock listener))
      (push accept-thread (listener-threads listener))))
  listener)

(defun stop-tcp-listener (listener)
  "Stop LISTENER: close the listen socket, close all accepted
connections, and join all threads.  Returns LISTENER."
  (unless (listener-running-p listener)
    (return-from stop-tcp-listener listener))
  (setf (listener-running-p listener) nil)
  ;; Close the listen socket to unblock the accept loop
  (handler-case
      (when (listener-listen-socket listener)
        (usocket:socket-close (listener-listen-socket listener)))
    (error () nil))
  ;; Close all accepted connections (unblocks receive loops)
  (let ((connections (bt:with-lock-held ((listener-lock listener))
                       (copy-list (listener-connections listener)))))
    (dolist (tr connections)
      (handler-case (transport-close tr)
        (error () nil))))
  ;; Join all threads (with a generous timeout via ignore-errors)
  (let ((threads (bt:with-lock-held ((listener-lock listener))
                   (copy-list (listener-threads listener)))))
    (dolist (thread threads)
      (when (and (bt:threadp thread) (bt:thread-alive-p thread))
        (handler-case (bt:join-thread thread)
          (error () nil)))))
  ;; Clear state
  (bt:with-lock-held ((listener-lock listener))
    (setf (listener-threads listener) nil)
    (setf (listener-connections listener) nil))
  (setf (listener-listen-socket listener) nil)
  listener)

;;; ---------------------------------------------------------------------
;;; Accept loop
;;; ---------------------------------------------------------------------

(defun tcp-accept-loop (listener)
  "Accept incoming TCP connections and spawn a receive loop for each.
Runs until the listener is stopped."
  (loop while (listener-running-p listener)
        do (handler-case
               ;; Wait for a connection with a timeout so we can
               ;; periodically check running-p
               (let ((ready (usocket:wait-for-input
                             (listener-listen-socket listener)
                             :timeout 1 :ready-only t)))
                 (when (and ready (listener-running-p listener))
                   (let* ((client-socket
                            (usocket:socket-accept
                             (listener-listen-socket listener)
                             :element-type '(unsigned-byte 8)))
                          (client-stream
                            (usocket:socket-stream client-socket))
                          (local-auth
                            (format nil "~A:~D"
                                    (listener-host listener)
                                    (listener-port listener)))
                          (tr (make-instance 'tcp-transport
                                            :authority "incoming"
                                            :local-authority local-auth
                                            :transforms (listener-transforms listener)
                                            :socket client-socket
                                            :stream client-stream))
                          (recv-thread
                            (bt:make-thread
                             (lambda ()
                               (tcp-receive-loop tr
                                                 (listener-runtime listener)))
                             :name "apis tcp-recv")))
                     (bt:with-lock-held ((listener-lock listener))
                       (push tr (listener-connections listener))
                       (push recv-thread (listener-threads listener))))))
             (error (e)
               (unless (listener-running-p listener)
                 (return))
               ;; Transient errors: log and continue
               (warn "TCP accept error: ~A" e)))))

;;; ---------------------------------------------------------------------
;;; Receive loop
;;; ---------------------------------------------------------------------

(defun tcp-receive-loop (transport runtime)
  "Read messages from TRANSPORT and deliver them locally in RUNTIME.
Runs until the connection is closed or an unrecoverable error occurs."
  (handler-case
      (loop
        (multiple-value-bind (envelope-string payload-string)
            (transport-receive transport)
          (let* ((msg (deserialize-message envelope-string payload-string))
                 (target (message-to msg)))
            (when target
              ;; Extract the worker-id from whatever address form the
              ;; TO field carries and deliver locally.
              (let ((worker-id
                      (etypecase target
                        (integer target)
                        (string
                         (multiple-value-bind (host port wid)
                             (parse-address target)
                           (declare (ignore host port))
                           wid)))))
                (deliver-locally msg worker-id runtime))))))
    (end-of-file ()
      ;; Clean shutdown: peer closed the connection
      (handler-case (transport-close transport) (error () nil)))
    (error (e)
      (declare (ignore e))
      (handler-case (transport-close transport) (error () nil)))))
