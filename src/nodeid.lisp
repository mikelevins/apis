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
;;; Apis uses v4uuid values as node identifiers. A node is a
;;; user account on a device that runs apis. Each message is identified
;;; by a 128-bit nodeid, a 64-bit session id, and a 64-bit messageid.
;;; Taken together these ids uniquely identify each message.
;;;
;;; The first time apis runs it generates and deposits a nodeid in a
;;; platform-specific directory.
;;;
;;; The location of the directory conforms to the XDG standard as follows:
;;; 
;;; | Operating System | Configuration Path                    | Data Path                      |
;;; |------------------|---------------------------------------|--------------------------------|
;;; | Linux            | $HOME/.config/apis/nodeid             | $HOME/.local/share/apis/nodeid |
;;; | macOS            | $HOME/Library/Preferences/apis/nodeid | $HOME/Library/apis/nodeid      |
;;; | Windows          | %APPDATA%\apis/nodeid                 | %APPDATA%\apis/nodeid          |

(defun make-nodeid (&optional (uuid-string nil))
  (if uuid-string
      (if (stringp uuid-string)
          (fuuid:from-string uuid-string)
          (error "Not a string: ~S" uuid-string))
      (fuuid:to-string (fuuid:make-v1))))
