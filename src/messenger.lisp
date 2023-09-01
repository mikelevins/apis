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

;;; ---------------------------------------------------------------------
;;; the messenger
;;; ---------------------------------------------------------------------

(define-condition unreadable-message-received ()
  ((message-bytes :reader message-bytes :initform nil :initarg :message-bytes)))

(defclass messenger ()
  ((receive-port :accessor messenger-receive-port :initform nil)
   (receive-socket :accessor messenger-receive-socket :initform nil)
   (receiver-process :accessor messenger-receiver-process :initform nil)
   (receive-queue :accessor messenger-receive-queue :initform nil)
   (receive-buffer :accessor messenger-receive-buffer :initform nil)
   (sender-process :accessor messenger-sender-process :initform nil)
   (send-queue :accessor messenger-send-queue :initform nil)
   (send-queue-occupied? :accessor send-queue-occupied?
                         :initform (bt:make-condition-variable :name "send-queue-occupied?"))
   (send-buffer :accessor messenger-send-buffer :initform nil))
  (:metaclass singleton-classes:singleton-class))

(defun the-messenger ()(make-instance 'messenger))

(defmethod clear-send-buffer () (fill (messenger-send-buffer (the-messenger)) 0))
(defmethod clear-receive-buffer () (fill (messenger-receive-buffer (the-messenger)) 0))
(defun clear-messenger-buffers ()(clear-send-buffer)(clear-receive-buffer))

(defun run-receiver (socket)
  (loop
     (usocket:wait-for-input socket)
     (multiple-value-bind (buffer size host port)
         (usocket:socket-receive socket (messenger-receive-buffer (the-messenger)) 
                                 (array-total-size (messenger-receive-buffer (the-messenger))))
       (queues:qpush (messenger-receive-queue (the-messenger)) 
                     (subseq buffer 0 size)))))

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
    (setf (messenger-sender-process (the-messenger))
          (bt:make-thread (lambda ()(run-sender))
                          :name (format nil "message sender")))))

(defun stop-messaging ()
  (let ((receiver (shiftf (messenger-receiver-process (the-messenger)) nil))
        (sender (shiftf (messenger-sender-process (the-messenger)) nil)))
    (when receiver
      (bt:destroy-thread receiver))
    (when sender
      (bt:destroy-thread sender))))

(defmethod send-message ((message message)(host string)(port integer))
  (let* ((msg-data (object->bytes message))
         (envelope (make-instance 'envelope :message-data msg-data :destination-host host :destination-port port)))
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
;;; (defparameter $msg1 (make-instance 'singleton-message :data '(1 2 3)))
;;; (send-message $msg1 *localhost* *message-receive-port*)
;;; (stop-messaging)
;;; (describe (messenger-receive-queue (the-messenger)))
;;; (describe (the-messenger))

;;; (setf $msgdata (queues:qpop (messenger-receive-queue (the-messenger))))
;;; (setf $msgobj (bytes->object $msgdata))
;;; (describe $msgobj)
;;; (describe (bytes->object (envelope-message-data $msgobj)))
