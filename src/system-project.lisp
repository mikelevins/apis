;;;; ***********************************************************************
;;;;
;;;; Name:          system-project.lisp
;;;; Project:       apis: worker bees for application hives
;;;; Purpose:       constructing an Apis project
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

;;; ---------------------------------------------------------------------
;;; build a project
;;; ---------------------------------------------------------------------

(defmethod write-package-file ((project-name string)(path pathname))
  (let ((file (merge-pathnames "package.lisp" path)))
    (with-open-file (out file :direction :output)
      (format out
              ";;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       ~A
;;;; Purpose:       
;;;; Author:        
;;;; Copyright:     
;;;;
;;;; ***********************************************************************

(defpackage #:~A
  (:use #:cl))
"
            project-name project-name))))

(defmethod write-package-file ((project-name string)(path string))
  (write-package-file project-name (pathname path)))

(defmethod write-readme-file ((project-name string)(path pathname))
  (let ((file (merge-pathnames "README.md" path)))
    (with-open-file (out file :direction :output)
      (format out
              "# ~A
 An apis project
"
              project-name))))

(defmethod write-readme-file ((project-name string)(path string))
  (write-readme-file project-name (pathname path)))

(defmethod write-system-definition ((project-name string)(path pathname))
    (let ((file (merge-pathnames (format nil "~A.asd" project-name) path)))
      (with-open-file (out file :direction :output)
        (format out
                ";;;; ***********************************************************************
;;;;
;;;; Name:          ~A.asd
;;;; Project:       ~A
;;;; Purpose:       system definitions       
;;;; Author:        
;;;; Copyright:     
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ---------------------------------------------------------------------
;;; system definitions
;;; ---------------------------------------------------------------------

(asdf:defsystem #:~A
  :description \"\"
  :author \"\"
  :license \"\"
  :serial t
  :depends-on ()
  :components ((:module \"src\"
                        :serial t
                        :components ((:file \"package\")
                                     ))))

;;; (asdf:load-system :~A)
"
project-name
project-name
project-name
project-name
))))

(defmethod write-system-definition ((project-name string)(path string))
  (write-system-definition project-name (pathname path)))

(defmethod populate-project ((path pathname))
  (assert (directory-pathname-p path)(path)
          "PATH argument must be a writable directory; found ~S" path)
  (let* ((project-name (first (last (pathname-directory path))))
         (srcdir (merge-pathnames "src/" path))
         (publicdir (merge-pathnames "public/" path))
         (assetsdir (merge-pathnames "assets/" publicdir))
         (imagesdir (merge-pathnames "images/" assetsdir))
         (jsdir (merge-pathnames "js/" assetsdir))
         (cssdir (merge-pathnames "css/" assetsdir)))
    (ensure-directories-exist srcdir)
    (ensure-directories-exist imagesdir)
    (ensure-directories-exist jsdir)
    (ensure-directories-exist cssdir)
    (write-system-definition project-name path)
    (write-readme-file project-name path)
    (write-package-file project-name srcdir)))

(defmethod populate-project ((path string))
  (populate-project (pathname path)))


(defmethod create-project ((path pathname))
  (let* ((path (pathname-as-directory path)))
    (ensure-directories-exist path)
    (if (directory (merge-pathnames "*.*" path))
        (error "A non-empty directory already exists at ~S" path)
        (populate-project path))))

(defmethod create-project ((path string))
  (create-project (pathname path)))

(in-package :apis)
;;; (create-project "/Users/mikel/Desktop/foo")

