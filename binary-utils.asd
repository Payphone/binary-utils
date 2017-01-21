;;;; binary-utils.asd

(asdf:defsystem #:binary-utils
  :description "Utilities for defining binary types and interacting with them."
  :author "Peyton Farrar <peyton@peytonfarrar.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "binary-utils")))
