;;;; ***********************************************************************
;;;;
;;;; Name:          nodeid.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       a simple, cheap method of generating v4 uuid strings
;;;;                for use in identifying Apis nodes.
;;;; Author:        mikel evins
;;;; Copyright:     2023 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.apis)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; Apis uses these v4uuid values as node identifiers. A node is a
;;; user account on a device that runs apis. The first time apis runs
;;; it generates and deposits a nodeid in a platform-specific directory.
;;;
;;; The location of the directory conforms to the XDG standard as follows:
;;; 
;;; | Operating System | Configuration Path                    | Data Path                      |
;;; |------------------|---------------------------------------|--------------------------------|
;;; | Linux            | $HOME/.config/apis/nodeid             | $HOME/.local/share/apis/nodeid |
;;; | macOS            | $HOME/Library/Preferences/apis/nodeid | $HOME/Library/apis/nodeid      |
;;; | Windows          | %APPDATA%\apis/nodeid                 | %APPDATA%\apis/nodeid          |

(defmethod deposit-v4-uuid-version ((int integer))
  (let ((int2 (dpb #b0100 (byte 4 76) int)))
    (dpb #b10 (byte 2 62) int2)))

#+nil (v4-uuid-bits->hex (dpb #b00 (byte 4 76) #xffffffffffffffffffffffffffffffff))
#+nil (v4-uuid-bits->hex (dpb #b10 (byte 2 62) #xffffffffffffffffffffffffffffffff))

(defun generate-128-random-bits ()
  (random #xffffffffffffffffffffffffffffffff *uuid-random-state*))

(defun generate-v4-uuid-bits ()
  (deposit-v4-uuid-version (generate-128-random-bits)))

#+nil (defparameter $bits (generate-v4-uuid-bits))
#+nil (format t "~%~(~x~)" $bits)
#+nil (length (format nil "~(~x~)" $bits))

(defmethod v4-uuid-bits->hex ((id integer))
  (format nil "~(~32,'0x~)" id))

#+nil (length (v4-uuid-bits->hex (generate-v4-uuid-bits)))

(defmethod v4-uuid-bits->uuid-string ((id-bits integer))
  (let* ((hex (v4-uuid-bits->hex id-bits)))
    (format nil "~A-~A-~A-~A-~A"
            (subseq hex 0 8)
            (subseq hex 8 12)
            (subseq hex 12 16)
            (subseq hex 16 20)
            (subseq hex 20))))


(defun make-nodeid (&optional int)
  (if int
      (progn (assert (typep int '(integer 0 #xfffffffffffffffffffffffffffffffe))()
                     "int must be an integer between 0 and ~D" #xfffffffffffffffffffffffffffffffe)
             (v4-uuid-bits->uuid-string (deposit-v4-uuid-version int)))
      (v4-uuid-bits->uuid-string (generate-v4-uuid-bits))))

#+nil (make-nodeid)
#+nil (make-nodeid 1111)
