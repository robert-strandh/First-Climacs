;;;  (c) copyright 2008 by
;;;           Troels Henriksen (athas@sigkill.dk)
;;;
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

;;; Implementation of structural editing commands for the Lisp syntax
;;; in Climacs. These commands were inspired by Paredit, which was
;;; originally written by Taylor "Riastradh" Campbell for GNU
;;; Emacs. In particular, many docstrings have been copied verbatim,
;;; then modified.

;;; This is a work in progress, be aware that problems are likely to
;;; arise, and that the editing commands are not yet completely
;;; comprehensive. Patches are, of course, welcome.

;;; You must do M-x Structedit Mode to enable these commands.

(cl:in-package #:climacs-structedit)

(drei:define-syntax-mode structedit-mode ()
    ()
    (:documentation "A mode for Paredit-style editing in Lisp syntax.")
    (:applicable-syntaxes lisp-syntax))

(drei:define-mode-toggle-commands com-structedit-mode
    (structedit-mode "Structedit")
    :command-table lisp-table)

(make-command-table 'structedit-table
 :errorp nil)

(defmethod syntax-command-tables append ((syntax structedit-mode))
  '(structedit-table))

(defun delete-form (buffer form)
  "Delete `form' from `buffer'."
  (drei-buffer:delete-buffer-range
   buffer (start-offset form) (drei-buffer:size form)))

(define-command (com-open-list :name t :command-table structedit-table)
    ((n 'integer :default 0))
  "Insert a balanced parenthesis pair.
With an argument N, put the closing parentheses after N
S-expressions forward.  If in string or comment, insert a single
opening parenthesis.  If in a character literal, replace the
character literal with #\(."
  (cond ((in-string-p (drei:current-syntax) (point))
         (insert-character #\())
        ((in-comment-p (drei:current-syntax) (point))
         (insert-character #\())
        ((in-character-p (drei:current-syntax) (point))
         (delete-form (esa:current-buffer)
                      (form-around (drei:current-syntax)
                                   (drei-buffer:offset (point))))
         (drei-buffer:insert-sequence (point) "#\\("))
        (t
         (when (and (not (zerop n))
                    (drei-motion:forward-expression
                     (point)
                     (drei:current-syntax) 1 nil))
           (drei-motion:backward-expression
            (point)
            (drei:current-syntax) 1 nil))
         (insert-character #\()
         (drei-motion:forward-expression (point) (drei:current-syntax) n nil)
         (insert-character #\))
         (drei-buffer:backward-object (point))
         (drei-motion:backward-expression
          (point)
          (drei:current-syntax) n nil))))

(define-command (com-wrap-expression :name t :command-table structedit-table)
    ((n 'integer :default 1))
  "Wrap the following N S-expressions in a list.
Automatically indent the newly wrapped S-expressions.  As a
special case, if the point is at the end of a list, simply insert
a pair of parentheses, rather than insert a lone opening
parenthesis and then signal an error, in the interest of
preserving structural validity."
  (com-open-list n))

(define-command (com-close-list-and-newline :name t :command-table structedit-table)
    ()
  "Move past one closing delimiter, add a newline, and reindent."
  (cond ((or (in-string-p (drei:current-syntax) (point))
             (in-comment-p (drei:current-syntax) (point)))
         (insert-character #\)))
        ((in-character-p (drei:current-syntax) (point))
         (delete-form (esa:current-buffer)
                      (form-around (drei:current-syntax)
                                   (drei-buffer:offset (point))))
         (drei-buffer:insert-sequence (point) "#\\)"))
        ((drei-motion:forward-up (point) (drei:current-syntax) 1 nil)
         (drei-buffer:insert-object (point) #\Newline)
         (indent-current-line (drei:current-view) (point)))))

(defun delete-object-structurally (delete-fn move-fn immediate-form-fn
                                   border-offset-fn
                                   at-border-fn)
  "Delete an object at `(point)' structurally. `Delete-fn' is
either `forward-delete-object' or `backward-delete-object',
`move-fn' is either `forward-object' or `backward-object',
`immediate-form-fn' is some form selector, `border-offset-fn' is
either `end-offset' or `begin-offset', `at-border-fn' is a
function used to determine whether or not `(point)' is at the end
of a structural object."
  (let ((immediate-form (funcall immediate-form-fn (drei:current-syntax)
                                 (drei-buffer:offset (point))))
        (form-around (form-around (drei:current-syntax)
                                  (drei-buffer:offset (point))))
        (list-at-mark (list-at-mark (drei:current-syntax) (point)))
        (string-at-mark (form-of-type-at-mark (drei:current-syntax) (point) #'form-string-p)))
    (cond ((and (or (form-string-p immediate-form)
                    (form-list-p immediate-form))
                (= (funcall border-offset-fn immediate-form)
                   (drei-buffer:offset (point))))
           (funcall move-fn (point)))
          ((and (funcall at-border-fn (drei:current-syntax) (point))
                form-around)
           (when (or (and list-at-mark
                          (null (form-children list-at-mark)))
                     (and string-at-mark
                          (= (drei-buffer:size string-at-mark) 2)))
             (delete-form (esa:current-buffer)
                          form-around)))
          ((and (form-character-p immediate-form)
                (= (funcall border-offset-fn immediate-form)
                   (drei-buffer:offset (point))))
           (delete-form (esa:current-buffer)
                        immediate-form))
          (t (funcall delete-fn (point))))))

(define-command (com-forward-delete-object-structurally
                 :name t :command-table structedit-table)
    ((force 'boolean :default nil))
  "Delete a character forward or move forward over a delimiter.
If on an opening S-expression delimiter, move forward into the
S-expression. If on a closing S-expression delimiter, refuse to
delete unless the S-expression is empty, in which case delete the
whole S-expression. If `force' is true, simply delete a character
forward, without regard for delimiter balancing."
  (if force
      (drei-editing:forward-delete-object (point))
      (delete-object-structurally #'drei-editing:forward-delete-object
                                  #'drei-buffer:forward-object
                                  #'form-after #'start-offset
                                  #'location-at-end-of-form)))

(define-command (com-backward-delete-object-structurally
                 :name t :command-table structedit-table)
    ((force 'boolean :default nil))
  "Delete a character backward or move backward over a delimiter.
If on an ending S-expression delimiter, move backward into the
S-expression. If on an opening S-expression delimiter, refuse to
delete unless the S-expression is empty, in which case delete the
whole S-expression. If `force' is true, simply delete a
character backward, without regard for delimiter balancing."
  (if force
      (drei-editing:backward-delete-object (point))
      (delete-object-structurally #'drei-editing:backward-delete-object
                                  #'drei-buffer:backward-object
                                  #'form-before #'end-offset
                                  #'location-at-beginning-of-form)))

(define-command (com-insert-double-quote-structurally
                 :name t :command-table structedit-table)
    ((n 'integer :default 0))
  "Insert a pair of double-quotes.
With a prefix argument N, wrap the following N S-expressions in
  double-quotes, escaping intermediate characters if necessary.
Inside a comment, insert a literal double-quote.
At the end of a string, move past the closing double-quote.
In the middle of a string, insert a backslash-escaped double-quote.
If in a character literal, replace the character literal with #\\\"."
  (cond ((in-comment-p (drei:current-syntax) (point))
         (insert-character #\"))
        ((at-end-of-string-p (drei:current-syntax) (point))
         (drei-buffer:forward-object (point)))
        ((in-string-p (drei:current-syntax) (point))
         (drei-buffer:insert-sequence (point) "\\\""))
        ((in-character-p (drei:current-syntax) (point))
         (delete-form (esa:current-buffer)
                      (form-around (drei:current-syntax)
                                   (drei-buffer:offset (point))))
         (drei-buffer:insert-sequence (point) "#\\\""))
        (t
         (let ((old-offset (drei-buffer:offset (point))))
           (drei-motion:forward-expression (point) (drei:current-syntax) n nil)
           (drei-buffer:insert-buffer-object (esa:current-buffer) old-offset #\")
           (insert-character #\")
           (drei-buffer:backward-object (point))
           (drei-motion:backward-expression
            (point)
            (drei:current-syntax) (min 1 n) nil)))))

(define-command (com-wrap-expression-in-doublequote :name t :command-table structedit-table)
    ((n 'integer :default 1))
  "Move to the end of the string, insert a newline, and indent.
If not in a string, act as `Insert Double Quote Structurally'; if
no prefix argument is specified, the default is to wrap one
S-expression, however, not zero."
  (if (in-string-p (drei:current-syntax) (point))
      (setf (drei-buffer:offset (point))
            (1+ (end-offset (form-around (drei:current-syntax) (point)))))
      (com-insert-double-quote-structurally n)))

(define-command (com-splice-list :name t :command-table structedit-table)
    ((kill-backward 'boolean :default nil))
  "Splice the list that the point is on by removing its delimiters.
With a prefix argument as in `C-u', kill all S-expressions
backward in the current list before splicing all S-expressions
forward into the enclosing list."
  (let ((list (list-at-mark (drei:current-syntax) (point))))
    (when list
      (let ((begin-mark (drei-buffer:make-buffer-mark (esa:current-buffer)
                                                      (start-offset list)))
            (end-mark (drei-buffer:make-buffer-mark (esa:current-buffer)
                                                    (end-offset list))))
        (when kill-backward
          (loop until (eq (list-at-mark (drei:current-syntax)
                                        (drei-buffer:offset (point)))
                          (or (form-before (drei:current-syntax)
                                           (drei-buffer:offset (point)))
                              (form-around (drei:current-syntax)
                                           (drei-buffer:offset (point)))))
             do (drei-editing:backward-delete-expression
                 (point)
                 (drei:current-syntax) 1 nil)))
        (drei-buffer:delete-buffer-range
         (esa:current-buffer)
         (drei-buffer:offset begin-mark) 1)
        (drei-buffer:delete-buffer-range
         (esa:current-buffer)
         (1- (drei-buffer:offset end-mark)) 1)))))

(define-command (com-kill-line-structurally :name t :command-table structedit-table)
    ()
  "Kill a line as if with \"Kill Line\", but respecting delimiters.
In a string, act exactly as \"Kill Line\" but do not kill past
the closing string delimiter.  On a line with no S-expressions on
it starting after the point or within a comment, act exactly as
\"Kill Line\".  Otherwise, kill all S-expressions that start
after the point."
  (let ((form-around (form-around (drei:current-syntax)
                                  (drei-buffer:offset (point))))
        (form-after (form-after (drei:current-syntax)
                                (drei-buffer:offset (point))))
        (comment (comment-at-mark (drei:current-syntax) (point))))
    (cond ((drei-base:empty-line-p (point))
           (drei-editing:forward-delete-object (point)))
          ((in-string-p (drei:current-syntax) (point))
           (if (= (drei-buffer:buffer-line-number
                   (esa:current-buffer)
                   (end-offset form-around))
                  (drei-buffer:line-number (point)))
               ;; Delete from point until the end of the string, but
               ;; keep the ending delimiter.
               (drei-base:kill-region (point) (1- (end-offset form-around)))
               ;; Delete from point until end of line.
               (drei-base:kill-region
                (point)
                (drei-buffer:end-of-line (drei-buffer:clone-mark (point))))))
          ((in-line-comment-p (drei:current-syntax) (point))
           ;; Delete until end of line
           (drei-base:kill-region
            (point)
            (drei-buffer:end-of-line (drei-buffer:clone-mark (point)))))
          ((in-long-comment-p (drei:current-syntax) (point))
           (if (= (drei-buffer:buffer-line-number
                   (esa:current-buffer)
                   (end-offset comment))
                  (drei-buffer:line-number (point)))
               ;; End of comment on same line as point, if a complete
               ;; long comment, don't delete the ending delimiter
               (drei-base:kill-region (point)
                                      (- (end-offset comment)
                                         (if (form-complete-p comment)
                                             2 0)))
               ;; Delete from point until end of line.
               (drei-base:kill-region
                (point)
                (drei-buffer:end-of-line (drei-buffer:clone-mark (point))))))
          ((and form-after
                (= (drei-buffer:buffer-line-number
                    (esa:current-buffer)
                    (start-offset form-after))
                   (drei-buffer:line-number (point))))
           (drei-editing:forward-kill-expression
            (point)
            (drei:current-syntax))
           (loop for form-after = (form-after (drei:current-syntax)
                                              (drei-buffer:offset (point)))
                 while (and form-after
                            (= (drei-buffer:buffer-line-number
                                (esa:current-buffer)
                                (start-offset form-after))
                               (drei-buffer:line-number (point))))
                 do (drei-editing:forward-kill-expression
                     (point)
                     (drei:current-syntax) 1 t)))
          (t (drei-editing:forward-kill-line
              (point)
              (drei:current-syntax) 1 t nil)))))

(esa:set-key
 `(com-open-list ,*numeric-argument-marker* ,*numeric-argument-marker*)
 'structedit-table
 '(#\())

(esa:set-key
 `(com-wrap-expression ,*numeric-argument-marker*)
 'structedit-table
 '((#\( :meta)))

(esa:set-key
 'com-close-list-and-newline
 'structedit-table
 '(#\)))

(esa:set-key
 `(com-forward-delete-object-structurally ,*numeric-argument-marker*)
 'structedit-table
 '((#\d :control)))

(esa:set-key
 `(com-backward-delete-object-structurally ,*numeric-argument-marker*)
 'structedit-table
 '((#\Backspace)))

(esa:set-key
 `(com-insert-double-quote-structurally ,*numeric-argument-marker*)
 'structedit-table
 '((#\")))

(esa:set-key
 `(com-wrap-expression-in-doublequote ,*numeric-argument-marker*)
 'structedit-table
 '((#\" :meta)))

(esa:set-key
 `(com-splice-list ,*numeric-argument-marker*)
 'structedit-table
 '((#\s :meta)))

(esa:set-key
 'com-kill-line-structurally
 'structedit-table
 '((#\k :control)))
