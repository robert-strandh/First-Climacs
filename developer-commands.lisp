;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)

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

;;; Commands for developing the Climacs editor. 

(cl:in-package #:climacs-commands)

(clim:define-command (com-reset-profile :name t :command-table climacs1-gui:development-table) ()
  #+sbcl (sb-profile:reset)
  #-sbcl nil)

(clim:define-command (com-report-profile :name t :command-table climacs1-gui:development-table) ()
  #+sbcl (sb-profile:report)
  #-sbcl nil)

(clim:define-command (com-recompile :name t :command-table climacs1-gui:development-table) ()
  (asdf:operate 'asdf:load-op :climacs))


(clim:define-gesture-name :select-other #+mcclim :pointer-button-press #-mcclim :pointer-button (:left :meta) :unique nil)

(clim:define-presentation-translator lisp-string-to-string
    (drei-lisp-syntax::lisp-string string climacs1-gui:development-table
                  :gesture :select-other
                  :tester-definitive t
                  :menu nil
                  :priority 10)
    (object)
  object)

(clim:define-command (com-accept-string :name t :command-table climacs1-gui:development-table) ()
  (esa:display-message (format nil "~s" (clim:accept 'string))))
 
(clim:define-command (com-accept-symbol :name t :command-table climacs1-gui:development-table) ()
  (esa:display-message (format nil "~s" (clim:accept 'symbol))))

(clim:define-command (com-accept-lisp-string :name t :command-table climacs1-gui:development-table) ()
  (esa:display-message (format nil "~s" (accept 'lisp-string))))
