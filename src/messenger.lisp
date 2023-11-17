;;;; ***********************************************************************
;;;;
;;;; Name:          messenger.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       message transport
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defparameter *localhost* "127.0.0.1")
(defparameter *message-receive-port* 10764)
(defparameter *maximum-buffer-size* 32767)
(defparameter +messenger-log-level+ '(:debug :info :none))
(defparameter *messenger-log-level* :info)

;;; ---------------------------------------------------------------------
;;; the messenger
;;; ---------------------------------------------------------------------
;;; a singleton messaging manager per process

(define-condition unreadable-message-received ()
  ((message-bytes :reader message-bytes :initform nil :initarg :message-bytes)))

(defclass messenger ()
  ((receive-port :accessor messenger-receive-port :initform nil)
   (receive-socket :accessor messenger-receive-socket :initform nil)
   (receiver-process :accessor messenger-receiver-process :initform nil)
   (receive-queue :accessor messenger-receive-queue :initform nil)
   (receive-buffer :accessor messenger-receive-buffer :initform nil)
   (local-delivery-process :accessor messenger-local-delivery-process :initform nil)
   (sender-process :accessor messenger-sender-process :initform nil)
   (send-queue :accessor messenger-send-queue :initform nil)
   (send-queue-occupied? :accessor send-queue-occupied?
                         :initform (bt:make-condition-variable :name "send-queue-occupied?"))
   (send-buffer :accessor messenger-send-buffer :initform nil)
   (known-agents :accessor messenger-known-agents :initform {}))
  (:metaclass singleton-classes:singleton-class))

(defun make-default-recipient-agent ()
  (make-instance 'agent :name :default-recipient))

(defmethod initialize-instance :after ((messenger messenger)
                                       &rest inits &key &allow-other-keys)
  (unless (gethash :default-recipient (messenger-known-agents messenger) nil)
    (let ((recip (make-default-recipient-agent)))
      (setf (gethash :default-recipient (messenger-known-agents messenger))
            recip)
      (apis::start-agent recip))))

(defun the-messenger ()(make-instance 'messenger))

(defmethod clear-send-buffer () (fill (messenger-send-buffer (the-messenger)) 0))
(defmethod clear-receive-buffer () (fill (messenger-receive-buffer (the-messenger)) 0))
(defun clear-messenger-buffers ()(clear-send-buffer)(clear-receive-buffer))

(defmethod find-known-agent ((name symbol))
  (gethash name (messenger-known-agents (the-messenger)) nil))

(defun list-known-agents ()
  (loop for key being the hash-keys in (messenger-known-agents (the-messenger))
     collect key))

(defmethod define-known-agent ((name symbol)(agent agent))
  (let ((old-agent (find-known-agent name)))
    (when old-agent (stop-agent old-agent)))
  (setf (gethash name (messenger-known-agents (the-messenger)))
        agent)
  agent)

(defun reset-default-recipient-agent ()
  (define-known-agent :default-recipient (make-default-recipient-agent)))

#+nil (find-known-agent :arbitrary)
#+nil (describe (find-known-agent :default-recipient))

(defun run-receiver (socket)
  (loop
     (usocket:wait-for-input socket)
     (multiple-value-bind (buffer size host port)
         (usocket:socket-receive socket (messenger-receive-buffer (the-messenger)) 
                                 (array-total-size (messenger-receive-buffer (the-messenger))))
       (queues:qpush (messenger-receive-queue (the-messenger)) 
                     (bytes->object (subseq buffer 0 size))))))

(defparameter *last-local-message-delivery* nil)

;;; default delivery if the datum is not an envelope
;;; (without an envelope we don't know that the destination agent is,
;;; so we choose the default recipient agent)
(defmethod deliver-message-to-agent (datum)
  (deliver-message datum (find-known-agent :default-recipient)))

(defmethod deliver-message-to-agent ((env envelope))
  (let* ((destination-agent-name (envelope-destination-agent env))
         (recipient (or (find-known-agent destination-agent-name)
                        (find-known-agent :default-recipient)))
         (contents (envelope-contents env)))
    (deliver-message contents recipient)
    (setf *last-local-message-delivery*
          (cons recipient env))))

(defun run-local-message-delivery ()
  (loop
     (sleep 0.125)
     (let ((next (queues:qpop (messenger-receive-queue (the-messenger)))))
       (when next
         (deliver-message-to-agent next)))))

(defun run-sender ()
  (loop
     (sleep 0.25)
     (do ((env (queues:qpop (messenger-send-queue (the-messenger)))
               (queues:qpop (messenger-send-queue (the-messenger)))))
         ;; when qpop returns nil, we've sent all the pending messages
         ((null env) 'done-sending)
       (let ((host (envelope-destination-host env))
             (port (envelope-destination-port env))
             (msg-bytes (object->bytes env)))
         (clear-send-buffer)
         (replace (messenger-send-buffer (the-messenger)) msg-bytes)
         (let ((out (usocket:socket-connect nil nil :protocol :datagram)))
           (usocket:socket-send out (messenger-send-buffer (the-messenger)) (length msg-bytes) :host host :port port))))))

(defun start-messaging (&optional (port *message-receive-port*))
  (let* ((agents-table (messenger-known-agents (the-messenger))))
    (unless (messenger-receive-queue (the-messenger))
      (setf (messenger-receive-queue (the-messenger))
            (make-instance 'queues:simple-cqueue)))
    (unless (messenger-receive-buffer (the-messenger))
      (setf (messenger-receive-buffer (the-messenger))
            (make-array *maximum-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0)))
    (unless (messenger-send-queue (the-messenger))
      (setf (messenger-send-queue (the-messenger))
            (make-instance 'queues:simple-cqueue)))
    (unless (messenger-send-buffer (the-messenger))
      (setf (messenger-send-buffer (the-messenger))
            (make-array *maximum-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0)))
    (clear-messenger-buffers)
    (let ((socket (usocket:socket-connect nil nil :protocol :datagram
                                          :local-host usocket:*wildcard-host* :local-port port)))
      (setf (messenger-receive-socket (the-messenger)) socket)
      (setf (messenger-receiver-process (the-messenger))
            (bt:make-thread (lambda ()
                              (unwind-protect (run-receiver socket)
                                (usocket:socket-close socket)))
                            :name (format nil "message receiver [~a]" port)))
      (setf (messenger-local-delivery-process (the-messenger))
            (bt:make-thread (lambda ()(run-local-message-delivery))
                            :name (format nil "local message delivery")))
      (setf (messenger-sender-process (the-messenger))
            (bt:make-thread (lambda ()(run-sender))
                            :name (format nil "message sender"))))
    (when agents-table
      (loop for key being the hash-keys in agents-table
         do (let ((agent (gethash key agents-table nil)))
              (when agent (start-agent agent)))))))

(defun stop-messaging ()
  (let ((receiver (shiftf (messenger-receiver-process (the-messenger)) nil))
        (receive-socket (messenger-receive-socket (the-messenger)))
        (sender (shiftf (messenger-sender-process (the-messenger)) nil))
        (agents-table (messenger-known-agents (the-messenger))))
    (when receiver
      (bt:destroy-thread receiver))
    (when receive-socket
      (usocket:socket-close receive-socket)
      (setf (messenger-receive-socket (the-messenger)) nil))
    (when sender
      (bt:destroy-thread sender))
    (when agents-table
      (loop for key being the hash-keys in agents-table
         do (let ((agent (gethash key agents-table nil)))
              (when agent (stop-agent agent)))))))

(defmethod send-message ((message message)(host string)(port integer) &optional (destination-agent nil))
  (let* ((envelope (make-instance 'envelope
                                  :contents message
                                  :destination-host host
                                  :destination-port port
                                  :destination-agent destination-agent)))
    (queues::qpush (messenger-send-queue (the-messenger)) envelope)))

(defun object->bytes (obj)
  (let* ((data (flexi-streams:with-output-to-sequence (out)
                 (cl-store:store obj out))))
    data))

;;; (type-of (object->bytes (list :list 1 "two" 3)))

(defmethod bytes->object ((bytes vector))
  (flexi-streams::with-input-from-sequence (in bytes)
    (cl-store:restore in)))

;;; (bytes->object (object->bytes (list :list 1 "two" 3)))

;;; (start-messaging)
;;; (defparameter $msg1 (make-instance 'singleton-message :operation nil :data '(1 2 3)))
;;; (defparameter $msg2 (make-instance 'singleton-message :operation :ping))
;;; (send-message $msg1 *localhost* *message-receive-port*)
;;; (send-message $msg2 *localhost* *message-receive-port*)
;;; (send-message $msg1 *localhost* *message-receive-port* :default-recipient)
;;; (send-message $msg1 "192.168.0.78" *message-receive-port*)
;;; (send-message $msg2 "192.168.0.159" *message-receive-port*)
;;; (describe (messenger-receive-queue (the-messenger)))
;;; (describe (messenger-send-queue (the-messenger)))
;;; (describe (the-messenger))
;;; (describe (find-known-agent :default-recipient))
;;; (stop-messaging)

;;; (setf $rmsg1 (queues:qpop (messenger-receive-queue (the-messenger))))
;;; (setf $rmsg2 (queues:qpop (messenger-receive-queue (the-messenger))))
;;; (describe $rmsg1)
;;; (describe $rmsg2)
;;; (describe (envelope-contents $rmsg1))
;;; (describe (envelope-contents $rmsg2))

;;; (find-known-agent :default-recipient)
;;; (messenger-known-agents (the-messenger))
;;; (reset-default-recipient-agent)
