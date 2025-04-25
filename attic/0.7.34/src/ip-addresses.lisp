;;;; ***********************************************************************
;;;;
;;;; Name:          ip-addresses.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       utilities for working with ip addresses
;;;; Author:        mikel evins
;;;; Copyright:     2015-2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)


(defun get-local-ips ()
  (let* ((local-interfaces (ip-interfaces:get-ip-interfaces))
         (local-ips (mapcar (lambda (intf)(ip-interfaces:ip-interface-address intf))
                            local-interfaces)))
    (loop for ip in local-ips
          collect (format nil "~A.~A.~A.~A"
                          (elt ip 0)
                          (elt ip 1)
                          (elt ip 2)
                          (elt ip 3)))))

#+nil (get-local-ips)

(defmethod localhost-ip-address? ((addr string))
  (or (and (equal "localhost" addr) t)
      (and (member addr (get-local-ips) :test #'equalp) t)))

#+nil (localhost-ip-address? "127.0.0.1")
#+nil (localhost-ip-address? "192.168.0.64")

