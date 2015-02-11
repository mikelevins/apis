;;;; ***********************************************************************
;;;;
;;;; Name:          delegate.lisp
;;;; Project:       Apis: the hive application
;;;; Purpose:       application delegate
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :apis)

(defclass apis-app-delegate (ns:ns-object)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/applicationDidFinishLaunching: :void) ((self apis-app-delegate) notification)
  )
