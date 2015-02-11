;;;; ***********************************************************************
;;;;
;;;; Name:          build.lisp
;;;; Project:       Apis: the hive application
;;;; Purpose:       build the application bundle
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ---------------------------------------------------------------------
;;; automatic parameters
;;; ---------------------------------------------------------------------
;;; Do not edit these:

(let* ((path *load-truename*)
       (project-root (make-pathname :directory (pathname-directory path))))
  ;;; when the app is delivered, we redefine path-base to resolve
  ;;; paths relative to the app bundle
  (defparameter *project-root* project-root))

;;; ---------------------------------------------------------------------
;;; load configuration parameters
;;; ---------------------------------------------------------------------

;;; load config.lisp, unless it doesn;t exist
;;; in that case, load config-template.lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (probe-file (merge-pathnames "config.lisp" *project-root*))
      (load (merge-pathnames "config.lisp" *project-root*))
      (load (merge-pathnames "config-template.lisp" *project-root*))))

;;; ---------------------------------------------------------------------
;;; locating project assets
;;; ---------------------------------------------------------------------

(defun project-assets-directory ()
  (merge-pathnames "assets/" *project-root*))

(defun bundle-directory ()
  (let* ((app-dirname (concatenate 'string *bundle-name* "/")))
    (merge-pathnames app-dirname *project-root*)))

(defun bundle-contents-directory ()
  (merge-pathnames "Contents/" (bundle-directory)))

(defun bundle-macos-directory ()
  (merge-pathnames "MacOS/" (bundle-contents-directory)))

(defun bundle-resource-directory ()
  (merge-pathnames "Resources/" (bundle-contents-directory)))


;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(defun build-bundle ()
  (ensure-directories-exist (bundle-macos-directory))
  (ensure-directories-exist (merge-pathnames "en.lproj/" (bundle-resource-directory)))
  (copy-file (merge-pathnames "bundle_resources/en.lproj/Credits.rtf" (project-assets-directory))
             (merge-pathnames "en.lproj/Credits.rtf" (bundle-resource-directory)))
  (copy-file (merge-pathnames "bundle_resources/en.lproj/InfoPlist.strings" (project-assets-directory))
             (merge-pathnames "en.lproj/InfoPlist.strings" (bundle-resource-directory)))
  (copy-file (merge-pathnames "bundle_resources/Info.plist" (project-assets-directory))
             (merge-pathnames "en.lproj/Info.plist" (bundle-resource-directory)))
  (copy-file (merge-pathnames "icon/Apis.icns" (project-assets-directory))
             (merge-pathnames "apis.icns" (bundle-resource-directory))))

