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

;;; Windows commands for the Climacs editor.

(cl:in-package #:climacs-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands for splitting windows

(defun split-window-maybe-cloning (vertically-p clone-current-view-p)
  #.(format nil "Split `(current-window)', vertically if `vertically-p'~@
                 is true, horizontally otherwise. If `clone-current-view-p'~@
                 is true, use a clone of `(current-view)' for the new window.")
  (handler-bind ((climacs1-gui:view-already-displayed
                   #'(lambda (condition)
                       (declare (ignore condition))
                       ;; If this happens, `clone-current-view-p' is false.
                       (esa:display-message
                        "Can't split: no view available for new window")
                       (return-from split-window-maybe-cloning nil))))
    (climacs1-gui:split-window vertically-p clone-current-view-p)))

(clim:define-command
    (com-split-window-vertically :name t :command-table climacs1-gui:window-table)
    ((clone-current-view 'boolean :default nil))
  (split-window-maybe-cloning t clone-current-view))

(esa:set-key `(com-split-window-vertically ,clim:*numeric-argument-marker*)
             'climacs1-gui:window-table
             '((#\x :control) (#\2)))

(clim:define-command
    (com-split-window-horizontally :name t :command-table climacs1-gui:window-table)
    ((clone-current-view 'boolean :default nil))
  (split-window-maybe-cloning nil clone-current-view))

(esa:set-key `(com-split-window-horizontally ,clim:*numeric-argument-marker*)
             'climacs1-gui:window-table
             '((#\x :control) (#\3)))

(clim:define-command
    (com-other-window :name t :command-table climacs1-gui:window-table)
    ()
  (climacs1-gui:other-window))

(esa:set-key 'com-other-window
             'climacs1-gui:window-table
             '((#\x :control) (#\o)))

(defun click-to-offset (window x y)
  (with-accessors ((top drei:top) (bot drei:bot)) (drei::view window)
    (let ((new-x (floor x (clim:stream-character-width window #\m)))
          (new-y (floor y (clim:stream-line-height window)))
          (buffer (esa-io:buffer (drei::view window))))
      (loop for scan from (drei-buffer:offset top)
            with lines = 0
            until (= scan (drei-buffer:offset bot))
            until (= lines new-y)
            when (eql (drei-buffer:buffer-object buffer scan) #\Newline)
              do (incf lines)
            finally (loop for columns from 0
                          until (= scan (drei-buffer:offset bot))
                          until (eql (drei-buffer:buffer-object buffer scan)
                                     #\Newline)
                          until (= columns new-x)
                          do (incf scan))
                    (return scan)))))

(clim:define-command
    (com-switch-to-this-window :name nil :command-table climacs1-gui:window-table)
    ((window 'clim:pane) (x 'integer) (y 'integer))
  (climacs1-gui:other-window window)
  (when (and (climacs1-gui:buffer-pane-p window)
             (typep (drei::view window) 'drei:point-mark-view))
    (setf (drei-buffer:offset (drei:point (drei::view window)))
          (click-to-offset window x y))))

(clim:define-presentation-to-command-translator
    blank-area-to-switch-to-this-window
    (clim:blank-area com-switch-to-this-window climacs1-gui:window-table :echo nil)
    (window x y)
  (list window x y))

(clim:define-gesture-name :select-other :pointer-button (:right) :unique nil)

(clim:define-command
    (com-mouse-save :name nil :command-table climacs1-gui:window-table)
    ((window 'clim:pane) (x 'integer) (y 'integer))
  (when (and (climacs1-gui:buffer-pane-p window)
             (eq window (esa:current-window)))
    (setf (drei-buffer:offset (drei-buffer:mark (drei::view window)))
          (click-to-offset window x y))
    (drei-commands::com-exchange-point-and-mark)
    (drei-commands::com-copy-region)))

(clim:define-presentation-to-command-translator
    blank-area-to-mouse-save
    (clim:blank-area com-mouse-save climacs1-gui:window-table
     :echo nil
     :gesture :select-other)
    (window x y)
  (list window x y))

(clim:define-gesture-name :middle-button :pointer-button (:middle) :unique nil)

(clim:define-command
    (com-yank-here :name nil :command-table climacs1-gui:window-table)
    ((window 'clim:pane) (x 'integer) (y 'integer))
  (when (climacs1-gui:buffer-pane-p window)
    (climacs1-gui:other-window window)
    (setf (drei-buffer:offset (drei:point (drei::view window)))
          (click-to-offset window x y))
    (drei-commands::com-yank)))

(clim:define-presentation-to-command-translator
    blank-area-to-yank-here
    (clim:blank-area com-yank-here climacs1-gui:window-table
     :echo nil
     :gesture :middle-button)
    (window x y)
  (list window x y))

(defun single-window ()
  (loop until (null (cdr (esa:windows clim:*application-frame*)))
        do (rotatef (car (esa:windows clim:*application-frame*))
                    (cadr (esa:windows clim:*application-frame*)))
           (com-delete-window))
  (setf *standard-output* (car (esa:windows clim:*application-frame*))))

(clim:define-command
    (com-single-window :name t :command-table climacs1-gui:window-table)
    ()
  (single-window))

(esa:set-key 'com-single-window
             'climacs1-gui:window-table
             '((#\x :control) (#\1)))

(clim:define-command
    (com-scroll-other-window :name t :command-table climacs1-gui:window-table)
    ()
  (let ((other-window (second (esa:windows clim:*application-frame*))))
    (when other-window
      (drei:page-down other-window (drei::view other-window)))))

(esa:set-key 'com-scroll-other-window
             'climacs1-gui:window-table
             '((#\v :control :meta)))

(clim:define-command
    (com-scroll-other-window-up :name t :command-table climacs1-gui:window-table)
    ()
  (let ((other-window (second (esa:windows clim:*application-frame*))))
    (when other-window
      (drei:page-up other-window (drei::view other-window)))))

(esa:set-key 'com-scroll-other-window-up
             'climacs1-gui:window-table
             '((#\V :control :meta)))

(clim:define-command
    (com-delete-window :name t :command-table climacs1-gui:window-table)
    ()
  (climacs1-gui:delete-window))

(esa:set-key 'com-delete-window
             'climacs1-gui:window-table
             '((#\x :control) (#\0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands for switching/killing current view.

(clim:define-command
    (com-switch-to-view :name t :command-table climacs1-gui:window-table)
    ;; Perhaps the default should be an undisplayed view?
    ((view 'clim:view
           :default
           (or (find (drei:current-view)
                     (drei-core:views clim:*application-frame*)
                     :test (complement #'eq))
               (climacs1-gui:any-view))))
  #.(format nil "Prompt for a view name and switch to that view. If a~@
                 view with that name does not exist, create a buffer-view~@
                 with the name and switch to it. Uses the name of the next~@
                 view (if any) as a default.")
  (handler-case (climacs-core:switch-to-view (esa:current-window) view)
    (climacs1-gui:view-already-displayed (condition)
      (climacs1-gui:other-window (climacs1-gui:window condition)))))

(esa:set-key `(com-switch-to-view ,clim:*unsupplied-argument-marker*)
             'climacs1-gui:window-table
             '((#\x :control) (#\b)))

(clim:define-command
    (com-kill-view :name t :command-table climacs1-gui:window-table)
    ((view 'clim:view :prompt "Kill view"
                      :default (drei:current-view)))
  #.(format nil "Prompt for a view name and kill that view. If the view~@
                 is of a buffer and the buffer needs saving, you will be~@
                 prompted to do so before killing it. Uses the current view~@
                 as a default.")
  (climacs-core:kill-view view))

(esa:set-key `(com-kill-view ,clim:*unsupplied-argument-marker*)
             'climacs1-gui:window-table
             '((#\x :control) (#\k)))

(esa-utils:define-menu-table
    climacs1-gui:window-menu-table (climacs1-gui:window-table)
  '(com-split-window-vertically nil)
  '(com-split-window-horizontally nil)
  'com-other-window
  'com-single-window
  'com-delete-window
  :divider
  `(com-switch-to-view ,clim:*unsupplied-argument-marker*)
  `(com-kill-view ,clim:*unsupplied-argument-marker*))
