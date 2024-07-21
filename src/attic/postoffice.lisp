;;;; ***********************************************************************
;;;;
;;;; Name:          postoffice.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       message transport
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; the postoffice
;;; ---------------------------------------------------------------------
;;; a singleton messaging manager per process

(define-condition unreadable-message-received ()
  ((message-bytes :reader message-bytes :initform nil :initarg :message-bytes)))

(defclass postoffice ()
  ((receive-port :accessor postoffice-receive-port :initform nil)
   (receive-socket :accessor postoffice-receive-socket :initform nil)
   (receiver-process :accessor postoffice-receiver-process :initform nil)
   (receive-queue :accessor postoffice-receive-queue :initform nil)
   (receive-buffer :accessor postoffice-receive-buffer :initform nil)
   (send-queue :accessor postoffice-send-queue :initform nil)
   (send-buffer :accessor postoffice-send-buffer :initform nil)
   (local-delivery-process :accessor postoffice-local-delivery-process :initform nil)
   (sender-process :accessor postoffice-sender-process :initform nil))
  (:metaclass singleton-class))

(defmethod initialize-instance :after ((postoffice postoffice)
                                       &rest inits &key &allow-other-keys)
  (declare (ignore inits))
  (setf (postoffice-receive-queue postoffice)
        (make-instance 'queues:simple-cqueue))
  (setf (postoffice-receive-buffer postoffice)
        (make-array *maximum-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0))
  (setf (postoffice-send-queue postoffice)
        (make-instance 'queues:simple-cqueue))
  (setf (postoffice-send-buffer postoffice)
        (make-array *maximum-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0))
  (clear-postoffice-buffers postoffice)
  (let ((socket (usocket:socket-connect nil nil :protocol :datagram
                                        :local-host usocket:*wildcard-host*
                                        :local-port *message-receive-port*)))
    (setf (postoffice-receive-socket postoffice) socket)
    (setf (postoffice-receiver-process postoffice)
          (bt:make-thread (lambda ()
                            (unwind-protect (run-receiver socket)
                              (usocket:socket-close socket)))
                          :name (format nil "message receiver")))
    (setf (postoffice-local-delivery-process postoffice)
          (bt:make-thread (lambda ()(run-local-delivery postoffice))
                          :name (format nil "local message delivery")))
    (setf (postoffice-sender-process postoffice)
          (bt:make-thread (lambda ()(run-sender postoffice))
                          :name (format nil "message sender")))))

(defun the-postoffice ()(make-instance 'postoffice))

(defmethod clear-send-buffer ((postoffice postoffice)) (fill (postoffice-send-buffer postoffice) 0))
(defmethod clear-receive-buffer ((postoffice postoffice)) (fill (postoffice-receive-buffer postoffice) 0))

(defmethod clear-postoffice-buffers ((postoffice postoffice))
  (clear-send-buffer postoffice)
  (clear-receive-buffer postoffice))

#+nil (describe (the-postoffice))

(defmethod run-sender ((postoffice postoffice))
  (loop
   (sleep 0.1)
   (do ((msg (queues:qpop (postoffice-send-queue postoffice))
             (queues:qpop (postoffice-send-queue postoffice))))
       ;; when qpop returns nil, we've sent all the pending messages
       ((null msg) 'done-sending)
     (let ((addr (message-to msg)))
       (if (delivery-address? addr)
           (let* ((addr (message-to msg))
                  (host (host addr))
                  (port (port addr))
                  (msg-bytes (object->bytes msg)))
             (clear-send-buffer postoffice)
             (replace (postoffice-send-buffer postoffice) msg-bytes)
             (let ((out (usocket:socket-connect nil nil :protocol :datagram)))
               (usocket:socket-send out
                                    (postoffice-send-buffer postoffice)
                                    (length msg-bytes)
                                    :host host
                                    :port port)))
           (file-dead-message msg))))))


(defun run-receiver (socket)
  (loop
   (usocket:wait-for-input socket)
   (multiple-value-bind (buffer size host port)
       (usocket:socket-receive socket (postoffice-receive-buffer postoffice) 
                               (array-total-size (postoffice-receive-buffer postoffice)))
     (queues:qpush (postoffice-receive-queue postoffice) 
                   (bytes->object (subseq buffer 0 size))))))

(defmethod run-local-delivery ((postoffice postoffice))
  (loop
   (sleep 0.125)
   (let ((next (queues:qpop (postoffice-receive-queue postoffice))))
     (when next
       (deliver-locally next)))))
