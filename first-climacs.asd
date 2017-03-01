;;;  (c) copyright 2004 by
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; ASDF system definition for Climacs.

(cl:in-package #:asdf-user)

(defsystem :first-climacs
  :depends-on (:mcclim :flexichain)
  :serial t
  :components
  ((:file "packages")
   (:file "text-syntax")
   ;; (:file "cl-syntax")
   ;; (:file "html-syntax")
   (:file "prolog-syntax")
   (:file "prolog2paiprolog")
   (:file "typeout")
   (:file "gui")
   (:file "core")
   (:file "io")
   (:file "groups")
   (:file "climacs")
   (:file "developer-commands")
  
   (:file "file-commands")
   (:file "misc-commands")
   (:file "search-commands")
   (:file "window-commands")
   (:file "climacs-lisp-syntax")
   (:file "climacs-lisp-syntax-commands")
   (:file "structured-editing")))
