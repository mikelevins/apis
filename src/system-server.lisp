;;;; ***********************************************************************
;;;;
;;;; Name:          apis.lisp
;;;; Project:       apis: worker bees for application hives
;;;; Purpose:       main program
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(load-plugins)

(defroute (:get "/")(request response)
  (send-response response :body "Welcome to apis"))

(def-directory-route "/" "/Users/mikel/Workshop/src/apis/public")

(defparameter *server* nil)

(defun start ()
  (unless *server*
    (as:with-event-loop ()
      ;; create a listener object, and pass it to start-server, which starts Wookie
      (let* ((listener (make-instance 'listener
                                      :bind nil  ; equivalent to "0.0.0.0" aka "don't care"
                                      :port 4242)))
        (setf *server* (start-server listener))
        ;; stop server on ctrl+c
        (as:signal-handler 2
                           (lambda (sig)
                             (declare (ignore sig))
                             ;; remove our signal handler (or the event loop will just sit indefinitely)
                             (as:free-signal-handler 2)
                             ;; graceful stop...rejects all new connections, but lets current requests
                             ;; finish.
                             (as:close-tcp-server *server*)
                             (setf *server* nil)))))))

(defun stop ()
  (when *server*
    (let ((server *server*))
      (as:free-signal-handler 2)
      (as:close-tcp-server server)
      (setf *server* nil)
      server)))

;;; (start)
