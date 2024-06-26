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
;;; A 64-bit identifier that combines:
;;; - the low 24 bits of the UTC time (counting the seconds of about the last 6 months)
;;; - 16 bits of counter, allowing us to generate up to 65535 IDs per second
;;; - a 24-bit random integer generated for each id

(defun makeid ()
  (let* ((time (get-universal-time)))
    (if (>= *session-id-counter* +max-session-id-counter+)
        (progn (setf *session-id-counter* 0)
               (loop until (not (= time (get-universal-time)))))
        (incf *session-id-counter*))
    (let ((time-bits (low-n-bits 24 (get-universal-time)))
          (random-bits (ironclad:strong-random #b111111111111111111111111)))
      (+ random-bits
         (ash *session-id-counter* 24)
         (ash time-bits 40)))))

#+nil (makeid)
#+nil (time (progn (setf $ids (loop for i from 0 below 100000 collect (makeid))) :done))
#+nil (length $ids)
#+nil (length (remove-duplicates $ids))
