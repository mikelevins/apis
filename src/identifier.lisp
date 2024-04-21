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



(defun timestamp-milliseconds ()
  "Returns the number of milliseconds elapsed since 1 January 2010 00:00:00 UTC."
  (- (get-universal-time) +apis-epoch+))


(defun makeid ()
  )

#+nil (makeid)
