(cl:in-package #:asdf-user)

(defsystem :first-climacs-prolog-syntax
  :depends-on (:first-climacs)
  :serial t
  :components
  ((:file "packages")
   (:file "prolog-syntax")
   (:file "prolog2paiprolog")))
