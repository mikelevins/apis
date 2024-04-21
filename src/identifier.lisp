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

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; A 64-bit identifier that combines:
;;; - the low 20 bits of the UTC time (counting the seconds of about the last 12 days)
;;; - 12 bits of counter, allowing us to generate up to 4096 IDs per second
;;; - a 32-bit random integer initialized once per id

(defun makeid ()
  (let* ((time (get-universal-time)))
    (if (>= *session-id-counter* +max-session-id-counter+)
        (progn (setf *session-id-counter* 0)
               (loop until (not (= time (get-universal-time)))))
        (incf *session-id-counter*))
    (let ((time-bits (low-n-bits 20 (get-universal-time)))
          (random-bits (random #b11111111111111111111111111111111)))
      (+ random-bits
         (ash *session-id-counter* 32)
         (ash time-bits 44)))))

#+nil (makeid)
#+nil (time (progn (setf $ids (loop for i from 0 below 10000 collect (makeid))) :done))
#+nil (length $ids)
#+nil (length (remove-duplicates $ids))
