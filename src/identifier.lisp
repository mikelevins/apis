;;;; ***********************************************************************
;;;;
;;;; Name:          identifier.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       unique ids for agents and messages
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; this version of makeid generates a 

(defun get-ip-address ()
  (let* ((interfaces (ip-interfaces:get-ip-interfaces))
         (addresses (mapcar 'ip-interfaces:ip-interface-address interfaces)))
    (find-if-not (lambda (addr)(equalp addr #(127 0 0 1)))
                 addresses)))

#+nil (get-ip-address)

(defun nodebits ()
  ;; the low-order 10 bits of the machine's private IP address
  (let* ((address (get-ip-address))
         (address-integer (if address
                              (reduce #'(lambda (n byte)
                                          (+ (* n 255) byte))
                                      address
                                      :initial-value 0)
                              ;; if we can't get an IP we just use 1023
                              #b1111111111)))
    (logand address-integer #b1111111111)))

#+nil (nodebits)

(defun timestamp-milliseconds ()
  "Returns the number of milliseconds elapsed since 1 January 2010 00:00:00 UTC."
  (- (get-universal-time) +apis-epoch+))

#+nil
(timestamp-milliseconds)

(defun makeid ()
  (let* ((nodebits (nodebits))
         (ts (timestamp-milliseconds))
         (random-bits (random (1+ #b111111111111))))
    (logior (ash 0 63)
            (ash ts 22)
            (ash nodebits 12)
            random-bits)))



;;; (time (makeid))
;;; (integer-length (makeid))
