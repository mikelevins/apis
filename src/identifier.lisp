;;;; ***********************************************************************
;;;;
;;;; Name:          identifier.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       unique ids for workers and messages
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; A 64-bit integer identifier that combines:
;;; - apis time (32-bit number of seconds since the Apis epoch)
;;; - the low 8 bits of the nodeid 
;;; - the low 8 bits of the PID 
;;; - 16 counter bits (allowing us to create 65535 ids per second)

(defun makeid ()
  (let* ((time (get-apis-time)))
    (if (>= *session-id-counter* +max-session-id-counter+)
        (progn (setf *session-id-counter* 0)
               (loop until (not (= time (get-apis-time)))))
        (incf *session-id-counter*))
    (let ((time-bits (get-apis-time))
          (node-bits (low-n-bits 8 +nodeid-integer+))
          (pid-bits (low-n-bits 8 (osicat-posix:getpid))))
      (+ *session-id-counter*
         (ash node-bits 16)
         (ash pid-bits 24)
         (ash time-bits 32)))))

#+nil (makeid)
#+nil (time (progn (setf $ids (loop for i from 0 below 1000000 collect (makeid))) :done))
#+nil (length $ids)
#+nil (length (remove-duplicates $ids))
#+nil (elt $ids 1000)
