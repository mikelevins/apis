;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
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

(defclass messenger ()
  ((receive-port :accessor messenger-receive-port :initform nil)
   (receive-socket :accessor messenger-receive-socket :initform nil)
   (server-process :accessor messenger-server-process :initform nil)
   (receive-queue :accessor messenger-receive-queue :initform nil)
   (receive-buffer :accessor messenger-receive-buffer :initform nil)
   (send-buffer :accessor messenger-send-buffer :initform nil))
  (:metaclass singleton-classes:singleton-class))

(defun the-messenger ()(make-instance 'messenger))

(defmethod clear-send-buffer () (fill (messenger-send-buffer (the-messenger)) 0))
(defmethod clear-receive-buffer () (fill (messenger-receive-buffer (the-messenger)) 0))
(defun clear-messenger-buffers ()(clear-send-buffer)(clear-receive-buffer))

(defun run-server (socket)
  (loop
     (usocket:wait-for-input socket)
     (multiple-value-bind (buffer size host port)
         (usocket:socket-receive socket (messenger-receive-buffer (the-messenger)) 
                                 (array-total-size (messenger-receive-buffer (the-messenger))))
       (queues:qpush (messenger-receive-queue (the-messenger)) 
                     (subseq buffer 0 size)))))

(defun start-messaging (&optional (port *message-receive-port*))
  (unless (messenger-receive-queue (the-messenger))
    (setf (messenger-receive-queue (the-messenger))
          (make-instance 'queues:simple-cqueue)))
  (unless (messenger-receive-buffer (the-messenger))
    (setf (messenger-receive-buffer (the-messenger))
          (make-array *maximum-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0)))
  (unless (messenger-send-buffer (the-messenger))
    (setf (messenger-send-buffer (the-messenger))
          (make-array *maximum-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0)))
  (clear-messenger-buffers)
  (let ((socket (usocket:socket-connect nil nil :protocol :datagram
                                        :local-host usocket:*wildcard-host* :local-port port)))
    (setf (messenger-receive-socket (the-messenger)) socket)
    (setf (messenger-server-process (the-messenger))
          (bt:make-thread (lambda ()
                            (unwind-protect (run-server socket)
                              (usocket:socket-close socket)))
                          :name (format nil "messaging server [~a]" port)))))

(defun stop-messaging ()
  (let ((server (shiftf (messenger-server-process (the-messenger)) nil)))
    (when server
      (bt:destroy-thread server))))

(defmethod send-message-data ((data vector)(host string)(port integer))
  (clear-send-buffer)
  (replace (messenger-send-buffer (the-messenger)) data)
  (let ((out (usocket:socket-connect nil nil :protocol :datagram)))
    (usocket:socket-send out (messenger-send-buffer (the-messenger)) (length data) :host host :port port)))

;;; (start-messaging)
;;; (class-of (messenger-receive-socket (the-messenger)))
;;; (send-message-data (vector 1 2 3 4) *localhost* *message-receive-port*)
;;; (send-message-data (vector 5 6 7) *localhost* *message-receive-port*)
;;; (send-message-data (vector 1 2 3 4) "explorersguild.com" *message-receive-port*)
;;; (stop-messaging)
;;; (describe (messenger-receive-queue (the-messenger)))
