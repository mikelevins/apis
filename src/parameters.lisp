;;;; ***********************************************************************
;;;;
;;;; Name:          parameters.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       system parameters
;;;; Author:        mikel evins
;;;; Copyright:     2015-2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defparameter *id-random-state* (make-random-state t))
(defparameter *default-message-time-to-live* 600) ; seconds
(defparameter *default-runtime-thread-count* 4)
(defparameter *default-runtime* nil)
(defparameter *dead-letters* (make-array 16 :initial-element nil :fill-pointer 0 :adjustable t))

;;; ---------------------------------------------------------------------
;;; Hardware thread detection
;;; ---------------------------------------------------------------------
;;; Queries the OS for the number of logical CPUs.  Uses only UIOP
;;; (bundled with ASDF) — no new dependencies.

(defun hardware-thread-count ()
  "Return the number of logical CPUs as an integer, or NIL if detection
fails.  Uses platform-specific commands via UIOP:RUN-PROGRAM:
  macOS  — sysctl -n hw.logicalcpu
  Linux  — nproc
  Windows — the NUMBER_OF_PROCESSORS environment variable.
Note: macOS is checked before generic Unix since it reports as both."
  (flet ((parse-int (string)
           (when (and string (plusp (length string)))
             (ignore-errors
              (parse-integer (string-trim '(#\Space #\Tab #\Newline #\Return)
                                          string))))))
    (cond
      ;; macOS — check before os-unix-p since macOS satisfies both.
      ((uiop:os-macosx-p)
       (parse-int
        (ignore-errors
         (uiop:run-program '("sysctl" "-n" "hw.logicalcpu")
                           :output :string))))
      ;; Windows
      ((uiop:os-windows-p)
       (parse-int (uiop:getenv "NUMBER_OF_PROCESSORS")))
      ;; Linux / other Unix
      ((uiop:os-unix-p)
       (parse-int
        (ignore-errors
         (uiop:run-program '("nproc")
                           :output :string))))
      (t nil))))

(defun default-thread-count ()
  "Return a reasonable default thread count for the runtime.  Uses the
heuristic (max 2 (floor (* N 3/4))) where N is the hardware thread
count.  Falls back to *DEFAULT-RUNTIME-THREAD-COUNT* (currently 4)
when hardware detection fails."
  (let ((hw (hardware-thread-count)))
    (if hw
        (max 2 (floor (* hw 3/4)))
        *default-runtime-thread-count*)))
