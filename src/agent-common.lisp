;;;; ***********************************************************************
;;;;
;;;; Name:          agent-common.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       agent implementation
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defclass agent ()
  ((id :reader agent-id :initform (makeid) :initarg :id)
   (lock :reader agent-lock :initform (bt:make-lock))
   (message-ready? :reader agent-message-ready? :initform (bt:make-condition-variable))
   (message-queue :accessor agent-message-queue :initform (make-instance 'queues:simple-cqueue))
   (event-process :accessor agent-event-process :initform nil)))


(defmethod handle-message ((agent agent) msg)
  (warn "You must specialize HANDLE-MESSAGE on a subclass of AGENT"))

(defmethod run-agent ((agent agent))
  (loop
     (bt:with-lock-held ((agent-lock agent))
       (loop for msg = (queues:qpop (agent-message-queue agent))
          then (queues:qpop (agent-message-queue agent))
          while msg
          do (handle-message agent msg))
       (bt:condition-wait (agent-message-ready? agent)
                          (agent-lock agent)))))

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
  (bt:with-lock-held ((agent-lock agent))
    (queues:qpush (agent-message-queue agent) msg)
    (bt:condition-notify (agent-message-ready? agent))))

;;; (defparameter $a (make-instance 'agent))
;;; (describe $a)
;;; (start-agent $a)
;;; (deliver-message (vector 1) $a)
;;; (deliver-message (vector 1 2) $a)
;;; (deliver-message (vector 1 2 3) $a)
;;; (stop-agent $a)

