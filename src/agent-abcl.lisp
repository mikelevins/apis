;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          agent.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defclass agent ()
  ((id :reader agent-id :initform (makeid) :initarg :id)
   (message-ready? :reader agent-message-ready? :initform nil)
   (message-queue :accessor agent-message-queue :initform (make-instance 'queues:simple-cqueue))
   (event-process :accessor agent-event-process :initform nil)))

(defmethod handle-message ((agent agent) msg)
  (format t "~%Agent ~S received message ~S" agent msg)
  (force-output t))

(defmethod run-agent ((agent agent))
  (loop
     (let ((msg (queues:qpop (agent-message-queue agent))))
       (if msg
           (handle-message agent msg)
           (error "need to go to sleep here until the next message arrives")))))


(defmethod make-agent-event-process ((agent agent))
  (bt:make-thread (lambda ()(run-agent agent))
                  :name (format nil "agent-event-process [~A]" agent)))

(defmethod start-agent ((agent agent))
  (setf (agent-event-process agent)
        (make-agent-event-process agent)))

(defmethod stop-agent ((agent agent))
  (let ((handler (shiftf (agent-event-process agent) nil)))
    (when handler
      (bt:destroy-thread handler))))

(defmethod deliver-message (msg (agent agent))
  (queues:qpush (agent-message-queue agent) msg)
  (error "need to signal here that something has arrived on the message queue"))

;;; (defparameter $a (make-instance 'agent))
;;; (describe $a)
;;; (start-agent $a)
;;; (deliver-message (vector 1) $a)
;;; (deliver-message (vector 1 2) $a)
;;; (deliver-message (vector 1 2 3) $a)
;;; (stop-agent $a)

