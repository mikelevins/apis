;;;; ***********************************************************************
;;;;
;;;; Name:          agent.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       agent implementation for abcl
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defclass agent ()
  ((id :reader agent-id :initform (makeid) :initarg :id)
   (message-ready? :reader agent-message-ready? :initform (ccl:make-semaphore))
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
           (ccl:wait-on-semaphore (agent-message-ready? agent))))))


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
  (ccl:signal-semaphore (agent-message-ready? agent)))

;;; (defparameter $a (make-instance 'agent))
;;; (describe $a)
;;; (start-agent $a)
;;; (deliver-message (vector 1) $a)
;;; (deliver-message (vector 1 2) $a)
;;; (deliver-message (vector 1 2 3) $a)
;;; (stop-agent $a)

