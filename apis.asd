;;;; apis.asd

(asdf:defsystem #:apis
  :description "a worker bee for application hives"
  :author "mikel evins <mikel@evins.net>"
  :license "Apache 2.0"
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "apis")))))



