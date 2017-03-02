;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2007 by
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

;;; miscellaneous commands for the Climacs editor.

(cl:in-package #:climacs-commands)

(clim:define-command
    (com-not-modified :name t :command-table climacs1-gui:buffer-table)
    ()
  "Clear the modified flag for the current buffer.
The modified flag is automatically set when the contents
of the buffer are changed. This flag is consulted, for instance,
when deciding whether to prompt you to save the buffer before killing it."
  (setf (esa-buffer:needs-saving (esa:current-buffer)) nil))

(esa:set-key 'com-not-modified
             'climacs1-gui:buffer-table
             '((#\~ :meta)))

(clim:define-command
    (com-what-cursor-position :name t :command-table drei:info-table)
    ()
  "Print information about point.
Gives the character after point (name and octal, decimal and hexidecimal charcode),
the offset of point, the total objects in the buffer,
and the percentage of the buffers objects before point.

FIXME: gives no information at end of buffer."
  (let* ((char (or (drei-buffer:end-of-buffer-p (drei:point))
                   (drei-buffer:object-after (drei:point))))
         (column (drei-buffer:column-number (drei:point))))
    (esa:display-message
     "Char: ~:[none~*~;~:*~:C (#o~O ~:*~D ~:*#x~X)~]~
      point=~D of ~D (~D%) column ~D"
     (and (characterp char) char)
     (and (characterp char) (char-code char))
     (drei-buffer:offset (drei:point))
     (drei-buffer:size (esa:current-buffer))
     (if (drei-buffer:size (esa:current-buffer))
         (round (* 100 (/ (drei-buffer:offset (drei:point))
                          (drei-buffer:size (esa:current-buffer)))))
         100)
     column)))

(esa:set-key 'com-what-cursor-position
             'drei:info-table
             '((#\x :control) (#\=)))

(clim:define-command
    (com-browse-url :name t :command-table climacs1-gui:base-table)
    ((url 'url :prompt "Browse URL"))
  (declare (ignorable url))
  #+ (and sbcl darwin) (sb-ext:run-program "/usr/bin/open" `(,url) :wait nil)
  #+ (and openmcl darwin) (ccl:run-program "/usr/bin/open" `(,url) :wait nil))

(clim:define-command
    (com-set-syntax :name t :command-table climacs1-gui:buffer-table)
    ((syntax 'drei-syntax:syntax
             :prompt "Name of syntax"))
  "Prompts for a syntax to set for the current buffer.
   Setting a syntax will cause the buffer to be reparsed using the new syntax."
  (drei-core:set-syntax (drei:current-view) syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Groups

(clim:define-command
    (com-define-group :name t :command-table climacs1-gui:global-climacs-table)
    ((name 'string :prompt "Name")
     (views '(sequence clim:view) :prompt "Views"))
  (when (or (not (climacs-core:get-group name))
            (clim:accept
             'boolean
             :prompt "Group already exists. Overwrite existing group?"))
    (climacs-core:add-group name views))
  (climacs-core:select-group (climacs-core:get-group name)))

(esa:set-key `(com-define-group ,clim:*unsupplied-argument-marker*
                                ,clim:*unsupplied-argument-marker*)
             'climacs1-gui:global-climacs-table
             '((#\x :control) (#\g) (#\d)))

(clim:define-command
    (com-define-file-group :name t
                           :command-table climacs1-gui:global-climacs-table)
    ((name 'string :prompt "Name")
     (pathnames '(sequence pathname) :prompt "Files"))
  (when (or (not (climacs-core:get-group name))
            (clim:accept
             'boolean
             :prompt "Group already exists. Overwrite existing group?"))
    (climacs-core:add-group name pathnames))
  (climacs-core:select-group (climacs-core:get-group name)))

(esa:set-key `(com-define-file-group
               ,clim:*unsupplied-argument-marker*
               ,clim:*unsupplied-argument-marker*)
             'climacs1-gui:global-climacs-table
             '((#\x :control) (#\g) (#\f)))

(clim:define-command
    (com-select-group :name t :command-table climacs1-gui:global-climacs-table)
    ((group 'climacs-core:group))
  (climacs-core:select-group group))

(esa:set-key `(com-select-group ,clim:*unsupplied-argument-marker*)
             'climacs1-gui:global-climacs-table
             '((#\x :control) (#\g) (#\s)))

(clim:define-command
    (com-deselect-group :name t :command-table climacs1-gui:global-climacs-table)
    ()
  (climacs-core:deselect-group)
  (esa:display-message "Group deselected"))

(esa:set-key 'com-deselect-group
             'climacs1-gui:global-climacs-table
             '((#\x :control) (#\g) (#\u)))

(clim:define-command
    (com-current-group :name t :command-table climacs1-gui:global-climacs-table)
    ()
  (esa:with-minibuffer-stream (s)
    (format s "Active group is: ")
    (clim:present (climacs-core:get-active-group) 'climacs-core:group :stream s)))

(esa:set-key 'com-current-group
             'climacs1-gui:global-climacs-table
             '((#\x :control) (#\g) (#\c)))

(clim:define-command
    (com-list-group-contents :name t
                             :command-table climacs1-gui:global-climacs-table)
    ()
  (esa:with-minibuffer-stream (s)
    (format s "Active group designates: ")
    (climacs-core:display-group-contents (climacs-core:get-active-group) s)))

(esa:set-key 'com-list-group-contents
             'climacs1-gui:global-climacs-table
             '((#\x :control) (#\g) (#\l)))
