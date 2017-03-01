(cl:in-package #:common-lisp-user)

(defpackage #:climacs-prolog-syntax
  (:use :clim-lisp :clim :drei-buffer :drei-base
	:drei-syntax :flexichain :drei :climacs-core :drei-fundamental-syntax
        :drei :esa-utils)
  (:shadow #:atom
           #:close
           #:exp
           #:integer
           #:open
           #:variable))
