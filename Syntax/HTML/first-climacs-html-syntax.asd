(cl:in-package #:asdf-user)

(defsystem :first-climacs-html-syntax
  :depends-on (:first-climacs)
  :serial t
  :components
  ((:file "packages")
   (:file "html-syntax")))
