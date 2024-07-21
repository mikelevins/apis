;;;; ***********************************************************************
;;;;
;;;; Name:          relayer.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       a worker that exchanges messages with remote relayers
;;;; Author:        mikel evins
;;;; Copyright:     2015-2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; CLASS relayer
;;; ---------------------------------------------------------------------

(defclass relayer ()
  ((receive-port :accessor receive-port :initform nil)
   (receive-socket :accessor receive-socket :initform nil)
   (receive-buffer :accessor receive-buffer
                   :initform (make-array *relayer-buffer-size*
                                         :element-type '(unsigned-byte 8)
                                         :initial-element 0))
   (receive-thread :accessor receive-thread :initform nil)
   (send-queue :accessor send-queue :initform nil)
   (send-buffer :accessor send-buffer
                :initform (make-array *relayer-buffer-size*
                                      :element-type '(unsigned-byte 8)
                                      :initial-element 0))
   (send-thread :accessor send-thread :initform nil)
   (send-semaphore :accessor send-semaphore :initform (bt:make-semaphore :name "send semaphore") ))
  (:metaclass singleton-class))

(defun the-relayer () (make-instance 'relayer))

(defmethod clear-receive-buffer ((relayer relayer))
    (fill (receive-buffer relayer) 0))

(defmethod clear-send-buffer ((relayer relayer))
    (fill (send-buffer relayer) 0))

(defmethod start-relayer ((relayer relayer)
                          &key (receive-port *message-receive-port*))
  (setf (receive-socket (the-relayer))
        (usocket:socket-connect nil nil :protocol :datagram
                                :local-host usocket:*wildcard-host*
                                :local-port receive-port))
  (setf (send-queue (the-relayer))
        (make-instance 'queues:simple-cqueue))
  (setf (receive-thread (the-relayer)) (make-relayer-receive-thread relayer))
  (setf (send-thread (the-relayer)) (make-relayer-send-thread relayer)))

(defmethod stop-relayer ((relayer relayer))
  (usocket:socket-close (receive-socket (the-relayer)))
  (setf (receive-socket (the-relayer)) nil)
  (clear-receive-buffer relayer)
  (bt:destroy-thread (receive-thread (the-relayer)))
  (setf (receive-thread (the-relayer)) nil)
  (queues:qclear (send-queue (the-relayer)))
  (setf (send-queue (the-relayer)) nil)
  (clear-send-buffer relayer)
  (bt:destroy-thread (send-thread (the-relayer)))
  (setf (send-thread (the-relayer)) nil))

(defmethod make-relayer-receive-thread ((relayer relayer))
  (bt:make-thread
   (lambda ()
     (loop
      (usocket:wait-for-input (receive-socket relayer))
      (multiple-value-bind (buffer size host port)
        (usocket:socket-receive (receive-socket relayer)
                                (receive-buffer relayer) 
                                (array-total-size (receive-buffer relayer)))
        (declare (ignore host port))
        (deliver-locally (bytes->object (subseq buffer 0 size))))))
   :name "relayer receive thread"))

(defmethod make-relayer-send-thread ((relayer relayer))
  (bt:make-thread
   (lambda ()
     (loop ; loop forever
      (bt:wait-on-semaphore (send-semaphore relayer))
      (let* ((msg (queues:qpop (send-queue relayer))))
        (when msg
          (let ((addr (message-to msg)))
            (if (delivery-address? addr)
                (let* ((addr (message-to msg))
                       (host (host addr))
                       (port (port addr))
                       (msg-bytes (object->bytes msg)))
                  (clear-send-buffer relayer)
                  (replace (send-buffer relayer) msg-bytes)
                  (let ((out (usocket:socket-connect nil nil :protocol :datagram)))
                    (usocket:socket-send out
                                         (send-buffer relayer)
                                         (length msg-bytes)
                                         :host host
                                         :port port)))
                (file-dead-message msg)))))))
   :name "relayer send thread"))
