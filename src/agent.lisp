;;;; ***********************************************************************
;;;;
;;;; Name:          agent.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       agent implementation for abcl
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; *agent-lock*
;;; ---------------------------------------------------------------------
;;; controls concurrent access to agents

(defparameter *agent-lock* (bt:make-lock))

;;; ---------------------------------------------------------------------
;;; CLASS agent
;;; ---------------------------------------------------------------------
;;; represents agents and agent processes

(defclass agent ()
  ((id :reader agent-id :initform (makeid) :initarg :id)
   (name :initform nil :initarg :name)
   (message-ready? :reader agent-message-ready? :initform (bt:make-condition-variable))
   (message-queue :accessor agent-message-queue :initform (make-instance 'queues:simple-cqueue))
   (event-process :accessor agent-event-process :initform nil)))

(defmethod agent-name ((agent agent))
  (or (slot-value agent 'name)
      (agent-id agent)))

(defmethod print-object ((obj agent) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~A" (agent-name obj))))

(defmethod handle-message-operation ((agent agent) (msg message) op)
  (format t "~%Agent ~S received message:~%  ~S~%" agent msg))

(defmethod handle-message-operation ((agent agent) (msg message)(op (eql :ping)))
  (format t "~%Agent ~S received :ping" agent))

(defmethod handle-message ((agent agent) (msg message))
  (let ((op (message-operation msg)))
    (handle-message-operation agent msg op)))

(defmethod handle-message ((agent agent) (env envelope))
  (handle-message agent (envelope-contents env)))

(defmethod handle-message ((agent agent) msg)
  (format t "Agent ~S received unrecognized message: ~S"
            agent (with-output-to-string (s)
                    (describe msg s))))

(defmethod make-agent-event-process ((agent agent))
  (bt:make-thread
   (lambda ()
     (loop
        (bt:with-lock-held (*agent-lock*)
          (loop for msg = (queues:qpop (agent-message-queue agent))
             then (queues:qpop (agent-message-queue agent))
             while msg
             do (handle-message agent msg))
          (bt:condition-wait (agent-message-ready? agent)
                             *agent-lock*))))
   :name (format nil "agent-event-process [~A]" agent)))

(defmethod start-agent ((agent agent))
  (setf (agent-event-process agent)
        (make-agent-event-process agent)))

(defmethod stop-agent ((agent agent))
  (let ((handler (shiftf (agent-event-process agent) nil)))
    (when handler
      (bt:destroy-thread handler))))

(defmethod agent-running? ((agent agent))
  (and (agent-event-process agent) t))

(defmethod deliver-message-to-agent (msg (agent agent))
  (bt:with-lock-held (*agent-lock*)
    (queues:qpush (agent-message-queue agent) msg)
    (bt:condition-notify (agent-message-ready? agent))))


;;; ---------------------------------------------------------------------
;;; SINGLETON-CLASS known-agents
;;; ---------------------------------------------------------------------
;;; keeps track of known agents for messaging and management purposes

(defclass known-agents ()
  ((roster :reader known-agents-roster :initform (make-hash-table :test 'eql)))
  (:metaclass singleton-class))

(defun the-known-agents ()(make-instance 'known-agents))

(defmethod find-known-agent ((name symbol))
  (gethash name (known-agents-roster (the-known-agents)) nil))

(defun list-known-agents ()
  (loop for key being the hash-keys in (known-agents-roster (the-known-agents))
     collect key))

(defun list-running-agents ()
  (let ((found nil))
    (loop for key being the hash-keys in (known-agents-roster (the-known-agents))
       using (hash-value val)
       do (when (agent-running? val)
            (pushnew key found)))
    found))

(defun list-stopped-agents ()
  (let ((found nil))
    (loop for key being the hash-keys in (known-agents-roster (the-known-agents))
       using (hash-value val)
       do (when (not (agent-running? val))
            (pushnew key found)))
    found))

(defmethod define-known-agent ((name symbol)(agent agent))
  (let ((old-agent (find-known-agent name)))
    (when old-agent (stop-agent old-agent)))
  (setf (gethash name (known-agents-roster (the-known-agents)))
        agent)
  agent)

(defmethod remove-known-agent ((name symbol))
  (let ((old-agent (find-known-agent name)))
    (when old-agent (stop-agent old-agent)))
  (remhash name (known-agents-roster (the-known-agents)))
  name)
