(cl:in-package #:asdf-user)

(defsystem :first-climacs-common-lisp-syntax
  :depends-on (:first-climacs)
  :serial t
  :components
  ((:file "packages")
   (:file "common-lisp-syntax")))
