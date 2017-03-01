(cl:in-package #:asdf-user)

(defsystem :first-climacs-ttcn3-syntax
  :depends-on (:first-climacs)
  :serial t
  :components
  ((:file "packages")
   (:file "ttcn3-syntax")))
