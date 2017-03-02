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

;;; Search commands for Climacs.

(cl:in-package #:climacs-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multiple query replace

(clim:make-command-table 'multiple-query-replace-drei-table :errorp nil)

(defun multiple-query-replace-find-next-match (mark re list)
  (multiple-value-bind (foundp start)
      (drei-base:re-search-forward mark re)
    (when foundp
      (loop with buffer = (esa-io:buffer mark)
            for string in list
            when (drei-base:buffer-looking-at buffer start string)
              do (return string)))))

(clim:define-command (com-multiple-query-replace :name t :command-table drei:search-table) ()
  "Prompts for pairs of strings, replacing the first with the second.
Entering an empty search string stops the prompting."
  (let ((strings
         (loop for string1 = (clim:accept 'string :prompt "Multiple Query Replace")
               until (string= string1 "")
               for string2
                 = (clim:accept 'string
                           :prompt (format nil
                                           "Replace ~A with"
                                           string1))
               collecting (cons string1 string2))))
    (multiple-query-replace strings)))

(clim:define-command (com-multiple-query-replace-from-buffer :name t :command-table drei:search-table)
    ((view 'clim:view :prompt "View with Query Repace strings"))
  (unless (member view (drei-core:views esa:*esa-instance*))
    (clim:beep)
    (esa:display-message "~A not an existing buffer" (esa-utils:name view))
    (return-from com-multiple-query-replace-from-buffer nil))
  (let* ((buffer (esa-io:buffer view))
         (contents (drei-buffer:buffer-substring buffer 0 (1- (drei-buffer:size buffer))))
         (strings (loop with length = (length contents)
                        with index = 0
                        with start = 0
                        while (< index length)
                        do (loop until (>= index length)
                                 while (drei-syntax:whitespacep (drei-syntax:syntax buffer)
                                                    (char contents index))
                                 do (incf index))
                           (setf start index)
                           (loop until (>= index length)
                                 until (drei-syntax:whitespacep (drei-syntax:syntax buffer)
                                                    (char contents index))
                                 do (incf index))
                        until (= start index)
                        collecting (string-trim '(#\Space #\Tab #\Newline)
                                                 (subseq contents start index)))))
    (unless (evenp (length strings))
      (clim:beep)
      (esa:display-message "Uneven number of strings in ~A" (esa-utils:name buffer))
      (return-from com-multiple-query-replace-from-buffer nil))
    (multiple-query-replace (loop for (string1 string2) on strings by #'cddr
                                  collect (cons string1 string2)))))

(clim:define-command (com-query-exchange :name t :command-table drei:search-table) ()
  "Prompts for two strings to exchange for one another."
  (let* ((string1 (clim:accept 'string :prompt "Query Exchange"))
         (string2 (clim:accept 'string :prompt (format nil
                                                  "Exchange ~A and"
                                                  string1))))
    (multiple-query-replace (list (cons string1 string2) (cons string2 string1)))))

(defun multiple-query-replace (strings)
  (declare (special strings))
  (let* ((occurrences 0)
         (search-strings (mapcar #'car strings))
         (re (format nil "~{~A~^|~}" search-strings)))
    (declare (special occurrences re))
    (when strings
      (let* ((point (drei:point))
             (found (multiple-query-replace-find-next-match point re search-strings)))
        (when found
          (setf (drei:query-replace-state (esa:current-window))
                (make-instance 'drei:query-replace-state
                 :string1 found
                 :string2 (cdr (assoc found strings :test #'string=)))
                (drei:query-replace-mode (esa:current-window))
                t)
          (esa:display-message "Replace ~A with ~A: "
                           (drei:string1 (drei:query-replace-state (esa:current-window)))
                           (drei:string2 (drei:query-replace-state (esa:current-window))))
          (esa:simple-command-loop 'multiple-query-replace-drei-table
                               (drei:query-replace-mode (esa:current-window))
                               ((setf (drei:query-replace-mode (esa:current-window)) nil))))))
    (esa:display-message "Replaced ~D occurrence~:P" occurrences)))

(clim:define-command (com-multiple-query-replace-replace
                 :name t
                 :command-table multiple-query-replace-drei-table)
    ()
  (declare (special strings drei:occurrences re))
  (let* ((point (clim:point (drei:current-view)))
         (state (drei:query-replace-state (esa:current-window)))
         (string1 (drei:string1 state))
         (string1-length (length string1)))
    (drei-buffer:backward-object point string1-length)
    (drei-core:replace-one-string point string1-length (drei:string2 state) (esa-utils:no-upper-p string1))
    (incf drei:occurrences)
    (let ((found (multiple-query-replace-find-next-match
                  point
                  re
                  (mapcar #'car strings))))
      (cond ((null found) (setf (drei:query-replace-mode (esa:current-window)) nil))
            (t (setf (drei:query-replace-state (esa:current-window))
                     (make-instance 'drei:query-replace-state
                        :string1 found
                        :string2 (cdr (assoc found strings :test #'string=))))
               (esa:display-message "Replace ~A with ~A: "
                                (drei:string1 (drei:query-replace-state (esa:current-window)))
                                (drei:string2 (drei:query-replace-state (esa:current-window)))))))))

(clim:define-command (com-multiple-query-replace-replace-and-quit
                 :name t
                 :command-table multiple-query-replace-drei-table)
    ()
  (declare (special strings drei:occurrences))
  (let* ((point (drei:point))
         (state (drei:query-replace-state (esa:current-window)))
         (string1 (drei:string1 state))
         (string1-length (length string1)))
    (drei-buffer:backward-object point string1-length)
    (drei-core:replace-one-string point string1-length (drei:string2 state) (esa-utils:no-upper-p string1))
    (incf drei:occurrences)
    (setf (drei:query-replace-mode (esa:current-window)) nil)))

(clim:define-command (com-multiple-query-replace-replace-all
                 :name t
                 :command-table multiple-query-replace-drei-table)
    ()
  (declare (special strings drei:occurrences re))
  (let* ((point (drei:point))
         (found nil))
    (loop for state = (drei:query-replace-state (esa:current-window))
          for string1 = (drei:string1 state)
          for string1-length = (length string1)
          do (drei-buffer:backward-object point string1-length)
             (drei-core:replace-one-string point
                     string1-length
                     (drei:string2 state)
                     (esa-utils:no-upper-p string1))
             (incf drei:occurrences)
             (setf found (multiple-query-replace-find-next-match
                                  point
                                  re
                                  (mapcar #'car strings)))
          while found
          do (setf (drei:query-replace-state (esa:current-window))
                   (make-instance 'drei:query-replace-state
                      :string1 found
                      :string2 (cdr (assoc found strings :test #'string=))))
          finally (setf (drei:query-replace-state (esa:current-window)) nil))))

(clim:define-command (com-multiple-query-replace-skip
                 :name t
                 :command-table multiple-query-replace-drei-table)
    ()
  (declare (special strings re))
  (let* ((point (drei:point))
         (found (multiple-query-replace-find-next-match
                 point
                 re
                 (mapcar #'car strings))))
    (cond ((null found) (setf (drei:query-replace-mode (esa:current-window)) nil))
            (t (setf (drei:query-replace-state (esa:current-window))
                     (make-instance 'drei:query-replace-state
                        :string1 found
                        :string2 (cdr (assoc found strings :test #'string=))))
               (esa:display-message "Replace ~A with ~A: "
                                (drei:string1 (drei:query-replace-state (esa:current-window)))
                                (drei:string2 (drei:query-replace-state (esa:current-window))))))))

(defun multiple-query-replace-set-key (gesture command)
  (clim:add-command-to-command-table command 'multiple-query-replace-drei-table
                                :keystroke gesture
                                :errorp nil))

(multiple-query-replace-set-key '(#\Newline) 'com-query-replace-exit)
(multiple-query-replace-set-key '(#\Space) 'com-multiple-query-replace-replace)
(multiple-query-replace-set-key '(#\Backspace) 'com-multiple-query-replace-skip)
(multiple-query-replace-set-key '(#\Rubout) 'com-multiple-query-replace-skip)
(multiple-query-replace-set-key '(#\q) 'com-query-replace-exit)
(multiple-query-replace-set-key '(#\y) 'com-multiple-query-replace-replace)
(multiple-query-replace-set-key '(#\n) 'com-multiple-query-replace-skip)
(multiple-query-replace-set-key '(#\.) 'com-multiple-query-replace-replace-and-quit)
(multiple-query-replace-set-key '(#\!) 'com-multiple-query-replace-replace-all)
