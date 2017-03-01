(cl:in-package #:asdf-user)

(defsystem :slidemacs
  :depends-on (:first-climacs)
  :serial t
  :components
  ((:file "slidemacs")
   (:file "slidemacs-gui")))
