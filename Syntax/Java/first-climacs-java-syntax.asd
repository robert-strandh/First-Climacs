(cl:in-package #:asdf-user)

(defsystem :first-climacs-java-syntax
  :depends-on (:first-climacs)
  :serial t
  :components
  ((:file "packages")
   (:file "java-syntax")
   (:file "java-syntax-commands")))
