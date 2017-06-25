;;;; ***********************************************************************
;;;;
;;;; Name:          apis.asd
;;;; Project:       apis: worker bees for application hives
;;;; Purpose:       apis system definition
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ---------------------------------------------------------------------
;;; system definitions
;;; ---------------------------------------------------------------------

(asdf:defsystem :apis
  :description "worker bees for application hives"
  :author "mikel evins <mikel@evins.net>"
  :license "Apache 2.0"
  :serial t
  :depends-on (:trivial-utf-8 :cl-fad :quri :cl-emb :wookie)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "system-paths")
                                     (:file "system-config")
                                     (:file "system-rendering")
                                     (:file "system-server")))))

(defun load-apis ()
  ;; tell wookie not to load SSL support
  (pushnew :wookie-no-ssl *features*)
  (asdf:load-system :wookie)
  ;; tell wookie to populate its plugin package before loading apis
  (funcall (intern "LOAD-PLUGINS"
                   (find-package :wookie)))
  (asdf:load-system :apis))

;;; (load-apis)
;;; (apis::start)
;;; (apis::stop)


