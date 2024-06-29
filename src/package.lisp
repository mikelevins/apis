;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2014-204 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage #:net.bardcode.apis
  (:use #:cl #:frugal-uuid :local-time :queues)
  (:nicknames :apis)
  (:export))
