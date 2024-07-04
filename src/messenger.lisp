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
   (default-recipient :accessor messenger-default-recipient
     :initform (make-instance 'worker :name :default-recipient)))
  (:metaclass singleton-class))

(defmethod initialize-instance :after ((messenger messenger)
                                       &rest inits &key &allow-other-keys)
  (apis::start-worker (messenger-default-recipient messenger)))

(defun the-messenger ()(make-instance 'messenger))

(defmethod clear-send-buffer () (fill (messenger-send-buffer (the-messenger)) 0))
(defmethod clear-receive-buffer () (fill (messenger-receive-buffer (the-messenger)) 0))
(defun clear-messenger-buffers ()(clear-send-buffer)(clear-receive-buffer))


(defun reset-default-recipient-worker ()
  (when (messenger-default-recipient (the-messenger))
    (stop-worker (messenger-default-recipient (the-messenger))))
  (setf (messenger-default-recipient (the-messenger))
        (make-instance 'worker :name :default-recipient))
  (unless (worker-running? (messenger-default-recipient (the-messenger)))
    (start-worker (messenger-default-recipient (the-messenger)))))

(defun run-receiver (socket)
  (loop
     (usocket:wait-for-input socket)
     (multiple-value-bind (buffer size host port)
         (usocket:socket-receive socket (messenger-receive-buffer (the-messenger)) 
                                 (array-total-size (messenger-receive-buffer (the-messenger))))
       (queues:qpush (messenger-receive-queue (the-messenger)) 
                     (bytes->object (subseq buffer 0 size))))))

(defmethod deliver-message ((msg message))
  (let* ((destination-worker-name (message-to-worker msg))
         (recipient (or (find-known-worker destination-worker-name)
                        (messenger-default-recipient (the-messenger)))))
    (deliver-message-to-worker msg recipient)))

(defun run-local-message-delivery ()
  (loop
     (sleep 0.125)
     (let ((next (queues:qpop (messenger-receive-queue (the-messenger)))))
       (when next
         (deliver-message next)))))

(defun run-sender ()
  (loop
     (sleep 0.1)
     (do ((msg (queues:qpop (messenger-send-queue (the-messenger)))
               (queues:qpop (messenger-send-queue (the-messenger)))))
         ;; when qpop returns nil, we've sent all the pending messages
         ((null msg) 'done-sending)
       (let ((host (message-to-host msg))
             (port (message-to-port msg))
             (msg-bytes (object->bytes msg)))
         (clear-send-buffer)
         (replace (messenger-send-buffer (the-messenger)) msg-bytes)
         (let ((out (usocket:socket-connect nil nil :protocol :datagram)))
           (usocket:socket-send out
                                (messenger-send-buffer (the-messenger))
                                (length msg-bytes)
                                :host host
                                :port port))))))

(defun start-messaging (&optional (port *message-receive-port*))
  (let* ((workers-table (known-workers-roster (the-known-workers))))
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
    (unless (messenger-default-recipient (the-messenger))
      (setf (messenger-default-recipient (the-messenger))
            (make-instance 'worker :name :default-recipient)))
    (unless (worker-running? (messenger-default-recipient (the-messenger)))
      (start-worker (messenger-default-recipient (the-messenger))))
    (when workers-table
      (loop for key being the hash-keys in workers-table
         do (let ((worker (gethash key workers-table nil)))
              (when worker (start-worker worker)))))))

(defun stop-messaging ()
  (let ((receiver (shiftf (messenger-receiver-process (the-messenger)) nil))
        (receive-socket (messenger-receive-socket (the-messenger)))
        (sender (shiftf (messenger-sender-process (the-messenger)) nil))
        (workers-table (known-workers-roster (the-known-workers))))
    (when receiver
      (bt:destroy-thread receiver))
    (when receive-socket
      (usocket:socket-close receive-socket)
      (setf (messenger-receive-socket (the-messenger)) nil))
    (when sender
      (bt:destroy-thread sender))
    (when (messenger-default-recipient (the-messenger))
      (stop-worker (messenger-default-recipient (the-messenger)))
      (setf (messenger-default-recipient (the-messenger)) nil))
    (when workers-table
      (loop for key being the hash-keys in workers-table
         do (let ((worker (gethash key workers-table nil)))
              (when worker (stop-worker worker)))))))

(defmethod send-message ((message message))
  (queues::qpush (messenger-send-queue (the-messenger)) message))

(defun object->bytes (obj)
  (let* ((data (flexi-streams:with-output-to-sequence (out)
                 (cl-store:store obj out))))
    data))


(defmethod bytes->object ((bytes vector))
  (flexi-streams::with-input-from-sequence (in bytes)
    (cl-store:restore in)))


