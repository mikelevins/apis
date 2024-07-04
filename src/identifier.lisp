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
;;; - 16 bits of counter, allowing us to generate up to 65535 IDs per second
;;; - a 16-bit random integer generated for each id

(defun makeid ()
  (let* ((time (get-apis-time)))
    (if (>= *session-id-counter* +max-session-id-counter+)
        (progn (setf *session-id-counter* 0)
               (loop until (not (= time (get-apis-time)))))
        (incf *session-id-counter*))
    (let ((time-bits (get-apis-time))
          (random-bits (ironclad:strong-random #b1111111111111111)))
      (+ random-bits
         (ash *session-id-counter* 16)
         (ash time-bits 32)))))

#+nil (makeid)
#+nil (time (progn (setf $ids (loop for i from 0 below 1000000 collect (makeid))) :done))
#+nil (length $ids)
#+nil (length (remove-duplicates $ids))
