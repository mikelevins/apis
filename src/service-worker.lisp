;;;; ***********************************************************************
;;;;
;;;; Name:          service-worker.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       a worker that continuously runs some code besides its message thread
;;;; Author:        mikel evins
;;;; Copyright:     2015-2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defclass service-worker (worker)
  ((service-thread :accessor service-thread :initform nil :initarg :service-thread)))

(defmethod start ((worker service-worker) &key thread-name service-function &allow-other-keys)
  (call-next-method)
  (let* ((thread-name (or thread-name (format nil "service thread (~X)" (id worker))))
         (thread (bt:make-thread
                  (lambda ()(funcall service-function worker))
                  :name thread-name)))
    (setf (service-thread worker) thread)))

(defmethod stop ((worker service-worker))
  (call-next-method)
  (let ((thread (shiftf (service-thread worker) nil)))
    (when thread
      (bt:destroy-thread thread))))

#+repl (defparameter $s1 (make-instance 'service-worker))
#+repl (describe $s1)
#+repl (start $s1
              :service-function (lambda (worker)
                                  (loop
                                    (progn (sleep 2)
                                           (format t "~%running ~S service thread" worker)))))
#+repl (send (message :from t :to $s1))
#+repl (stop $s1)
