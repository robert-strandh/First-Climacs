(cl:in-package #:common-lisp-user)

(defpackage #:climacs-java-syntax
  (:use :clim-lisp :clim :clim-extensions :drei-buffer :drei-base
	:drei-syntax :drei-fundamental-syntax :flexichain :drei
	:drei-motion :drei-editing :esa-utils :esa :drei-core :esa-io
	:drei-lr-syntax)
  (:shadow clim:form)
  (:export #:java-syntax)
  (:documentation "Implementation of the syntax module used for
editing Java(tm) code."))
