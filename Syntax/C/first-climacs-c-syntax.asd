(cl:in-package #:asdf-user)

(defsystem :first-climacs-c-syntax
  :depends-on (:first-climacs)
  :serial t
  :components
  ((:file "c-syntax")
   (:file "c-syntax-commands")))
