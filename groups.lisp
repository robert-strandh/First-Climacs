;;;  (c) copyright 2006-2007 by
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

;;; Implementation of a groups concept.

(cl:in-package #:climacs-core)

(defvar *persistent-groups* (make-hash-table :test #'equal)
  #.(format nil "A hash table of groups that are persistent~@
                 across invocations of the Climacs editor.~@
                 Typically, these do not designate concrete~@
                 pathnames, but contain more abstract designations~@
                  such as \"all files in the current directory\"."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File/View group classes.

(defclass group (esa-utils:name-mixin)
  ())

(defclass group-element (group)
  ((%element :initarg :element :initform nil :reader element))
  (:documentation "Group class denoting a single element"))

(defclass standard-group (group)
  ((%elements :initarg :elements :initform nil :reader elements))
  (:documentation "Group class denoting a sequence of elements."))

(defclass current-view-group (group)
  ()
  (:documentation "Group class denoting the currently active view."))

(defclass synonym-group (group)
  ((%other-name :initarg :other-name
                :initform (error "The name of another group must be provided")
                :reader other-name))
  (:documentation #.(format nil "Group class that forwards all methods to a~@
                                 group with a specific name.")))

(defclass custom-group (group)
  ((%list-pathnames-lambda
    :initarg :pathname-lister
    :initform
    (error "A custom group must have code for retrieving a list of pathnames")
    :reader pathname-lister)
   (%select-group-lambda
    :initarg :select-response
    :initform #'(lambda (&rest a)
                   (declare (ignore a)))
    :reader select-response)
   (%value-plist
    :initform nil
    :accessor value-plist))
  (:documentation
   #.(format nil "A group that will call a provided function~@
                  when it is selected or asked for pathnames.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The group protocol.

(defgeneric group-views (group)
  (:documentation
   #.(format nil "Get a list of views in GROUP. Only already existing~@
                  views will be returned, use ENSURE-GROUP-VIEWS if~@
                  you want all views defined by the group.")))

(defgeneric ensure-group-views (group)
  (:documentation
   #.(format nil "For each pathname in GROUP that does not have a~@
                  corresponding view, open a view for that pathname.")))

(defgeneric select-group (group)
  (:documentation
   #.(format nil "Tell the group object GROUP that the user has~@
                  selected it. This method is responsible for~@
                  setting the active group. If GROUP needs additional~@
                  information, it should query the user when this method~@
                  is invoked. The standard method should be sufficient~@
                  for most group classes."))
  (:method ((group group))
    ;; Use a synonym group so that changes to the group of this name
    ;; will be reflected in the active group.
    (setf (climacs1-gui:active-group clim:*application-frame*)
          (make-synonym-group group))))

(defgeneric display-group-contents (group stream)
  (:documentation
   #.(format nil "Display the contents of GROUP to STREAM. Basically,~@
                  this should describe which views or files would be~@
                  affected by group-aware commands if GROUP was the~@
                  active group. There is no standard format for the output,~@
                  but it is intended for displaying to the user.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Protocol implementation.

;;; Display helper functions.
(defun normalise-group-element (element)
  #.(format nil "Turn ELEMENT into either a pathname, an existing~@
                 view or NIL. If a pathname is returned, it is assumed~@
                 to be safe to find the file with that name.")
  (typecase element
    (drei:drei-view
     (find element (drei-core:views clim:*application-frame*)))
    ((or pathname string)
     (or (find-view-with-pathname (pathname element))
         (when (findablep element)
           element)))
    (group-element
     (normalise-group-element (element element)))))

(defun display-group-element (element stream)
  (let ((norm-element (normalise-group-element element)))
    (typecase norm-element
      (drei:drei-view
       (clim:present norm-element 'clim:view :stream stream))
      ((or pathname string)
       (clim:present norm-element 'pathname :stream stream)))))

;;; Singular group elements.
(defmethod group-views ((group group-element))
  (let ((element (element group)))
    (cond ((and (typep element 'drei:drei-view)
                (find element (drei-core:views clim:*application-frame*)))
           (list element))
          ((or (pathnamep element)
               (stringp element))
           (let ((view (find-view-with-pathname (pathname element))))
             (when view (list view))))
          (t '()))))

(defmethod ensure-group-views ((group group-element))
  (typecase (element group)
    (drei:drei-view
     (unless (find (element group) (drei-core:views clim:*application-frame*))
       (ensure-open-file (pathname (esa-buffer:filepath (element group))))))
    (pathname
     (ensure-open-file (element group)))
    (string
     (ensure-open-file (pathname (element group))))))

(defmethod display-group-contents ((group group-element)
                                   (stream clim:extended-output-stream))
  (display-group-element (element group) stream))

;;; Standard sequence groups.
(defmethod group-views ((group standard-group))
  (apply #'append (mapcar #'group-views (elements group))))

(defmethod ensure-group-views ((group standard-group))
  (mapcar #'ensure-group-views (elements group)))

(defmethod display-group-contents ((group standard-group)
                                   (stream clim:extended-output-stream))
  (clim:present (remove-if #'null
                           (mapcar #'normalise-group-element (elements group)))
                '(sequence (or pathname clim:view)) :stream stream))

;;; The current view group (default).
(defmethod group-views ((group current-view-group))
  (list (drei:current-view)))

(defmethod ensure-group-views ((group current-view-group))
  nil)

(defmethod display-group-contents ((group current-view-group)
                                   (stream clim:extended-output-stream))
  (display-group-element (drei:current-view) stream))

;;; Custom groups.
(defmethod group-views ((group custom-group))
  (let ((lister (pathname-lister group)))
    (remove-if #'null
               (mapcar #'find-view-with-pathname (funcall lister group)))))

(defmethod ensure-group-views ((group custom-group))
  (mapcar #'ensure-open-file (funcall (pathname-lister group) group)))

(defmethod select-group ((group custom-group))
  (funcall (select-response group) group)
  (setf (climacs1-gui:active-group clim:*application-frame*) group))

(defmethod display-group-contents ((group custom-group)
                                   (stream clim:extended-output-stream))
  (let ((lister (pathname-lister group)))
    (clim:present
     (remove-if #'null
                (mapcar #'normalise-group-element (funcall lister group)))
     '(sequence (or pathname clim:view)) :stream stream)))

;;; Synonym groups.

(define-condition group-not-found (simple-error)
  ((%group-name :reader group-name
                :initarg :group-name
                :initform (error "A name for the group must be provided")))
  (:report (lambda (condition stream)
             (format stream "Group named ~a not found" (group-name condition))))
  (:documentation
   #.(format nil "This condition is signaled whenever a synonym group~@
                  is unable to find the group that it is supposed to~@
                  forward method invocations to.")))

(defmethod group-views ((group synonym-group))
  (if (get-group (other-name group))
      (group-views (get-group (other-name group)))
      (error 'group-not-found :group-name (other-name group))))

(defmethod ensure-group-views ((group synonym-group))
  (if (get-group (other-name group))
      (ensure-group-views (get-group (other-name group)))
      (error 'group-not-found :group-name (other-name group))))

(defmethod select-group ((group synonym-group))
  (if (get-group (other-name group))
      (select-group (get-group (other-name group)))
      (error 'group-not-found :group-name (other-name group))))

(defmethod display-group-contents ((group synonym-group) stream)
  (if (get-group (other-name group))
      (display-group-contents (get-group (other-name group)) stream)
      (error 'group-not-found :group-name (other-name group))))

;;; Util stuff.
(defun make-synonym-group (group)
  #.(format nil "Create and return a synonym group that refers~@
                 to GROUP. All group protocol-specified methods~@
                 called on the synonym group will be forwarded to~@
                 a group with the same name as GROUP.")
  (make-instance 'synonym-group
    :other-name (esa-utils:name group)
    :name (esa-utils:name group)))

(defun make-group-element (object)
  "Make a `group-element' object containg `object' as element."
  (make-instance 'group-element :element object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interface

(defun add-group (name elements)
  #.(format nil "Define a group called NAME (a string) containing~@
                 the elements ELEMENTS, which must be a list of~@
                 pathnames and/or views, and add it to the list of~@
                 defined groups.")
  (setf (gethash name (climacs1-gui:groups clim:*application-frame*))
        (make-instance 'standard-group
          :name name
          :elements (mapcar #'make-group-element elements))))

(defun get-group (name)
  "Return the group with the name `name'."
  (or (gethash name (climacs1-gui:groups clim:*application-frame*))
      (gethash name *persistent-groups*)))

(defun get-active-group ()
  "Return the currently active group."
  (or (climacs1-gui:active-group clim:*application-frame*)
      (deselect-group)))

(defun deselect-group ()
  "Deselect the currently active group."
  (setf (climacs1-gui:active-group clim:*application-frame*)
        (make-instance 'current-view-group
          :name "none")))

(defmacro with-group-views ((views group &key keep) &body body)
  #.(format nil "Make sure that all files designated by GROUP are~@
                 open in views during the evaluation of BODY. If~@
                 KEEP is NIL, all views created by this macro will~@
                 be saved and killed after BODY has run. Also, VIEWS~@
                 will be bound to a list of the views containing the~@
                 files designated by GROUP while BODY is run.")
  (esa-utils:with-gensyms (views-before views-after view-diff)
    (esa-utils:once-only (group keep)
      `(let ((,views-before (drei-core:views clim:*application-frame*))
             (,group ,group))
         (ensure-group-views ,group)
         (let* ((,views-after (drei-core:views clim:*application-frame*))
                (,view-diff (set-difference ,views-after ,views-before))
                (,views (group-views ,group)))
           (unwind-protect (progn ,@body)
             (unless ,keep
               (loop for view in ,view-diff
                  do (save-view view)
                  do (kill-view view)))))))))

(defmacro define-group (name (group-arg &rest args) &body body)
  #.(format nil "Define a persistent group named NAME. BODY should~@
                 return a list of pathnames and will be used to~@
                 calculate which files are designated by the group.~@
                 ARGS should be two-element lists, with the first~@
                 element bound to the result of evaluating the second~@
                 element. The second element will be evaluated when the~@
                 group is selected to be the active group by the user.")
  (esa-utils:with-gensyms (group)
    (esa-utils:once-only (name)
      `(let ((,name ,name))
         (assert (stringp ,name))
         (setf (gethash ,name *persistent-groups*)
               (make-instance 'custom-group
                 :name ,name
                 :pathname-lister
                 #'(lambda (,group)
                     (destructuring-bind
                         (&key ,@(mapcar #'(lambda (arg)
                                             `((,arg ,arg)))
                                         (mapcar #'first args)))
                         (value-plist ,group)
                       (let ((,group-arg ,group))
                         ,@body)))
                 :select-response
                 #'(lambda (group)
                     (declare (ignorable group))
                     ,@(loop for (name form) in args
                             collect `(setf (getf (value-plist group) ',name)
                                            ,form)))))))))

(define-group "Current Directory Files" (group)
  (declare (ignore group))
  (directory (make-pathname
              :directory (pathname-directory (esa-buffer:filepath (drei:current-view)))
              :name :wild
              :type :wild)))

(define-group "Directory Files"
    (group (directory (clim:accept 'pathname
                                   :prompt "Directory"
                                   :default (directory-of-buffer (esa-io:buffer (drei:current-view)))
                                   :insert-default t)))
  (declare (ignore group))
  (directory (make-pathname :directory (pathname-directory directory)
                            :name :wild
                            :type :wild)))

(define-group "Directory Lisp Files"
    (group (directory (clim:accept 'pathname
                                   :prompt "Directory"
                                   :default (directory-of-buffer (esa-io:buffer (drei:current-view)))
                                   :insert-default t)))
  (declare (ignore group))
  (directory (make-pathname :directory (pathname-directory directory)
                            :name :wild
                            :type "lisp")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLIM interface stuff.

(clim:define-presentation-method clim:accept
    ((type group) stream view &key (default nil defaultp)
     (default-type type))
  (multiple-value-bind (object success string)
      (clim:complete-input
       stream
       (lambda (so-far action)
         (clim:complete-from-possibilities
          so-far
          (append (loop with groups = (climacs1-gui:groups clim:*application-frame*)
                        for key being the hash-keys of groups
                        collecting key)
                  (loop for key being the hash-keys of *persistent-groups*
                        collecting key))
          '(#\Space)
          :action action
          :name-key #'identity
          :value-key #'identity))
       :partial-completers '(#\Space)
       :allow-any-input nil)
    (cond (success
           (values (get-group object) type))
          ((and (zerop (length string)) defaultp)
           (values default default-type))
          (t (values string 'string)))))

(clim:define-presentation-method clim:present
    (object (type group) stream view &key)
  (let ((name (esa-utils:name object)))
    (princ name stream)))

(clim:define-presentation-method clim:present
    ((object synonym-group) (type group) stream view &key)
  (if (get-group (other-name object))
      (clim:present (get-group (other-name object)) type :stream stream :view view)
      (error 'group-not-found :group-name (other-name object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Now hook it all up.

(defclass group-target-specification (drei-core:view-list-target-specification)
  ((%group :initarg :group
           :reader group
           :initform
           (error "A group must be provided for a group target specification")))
  (:documentation #.(format nil "The target-specification class used~@
                                 for groups in Climacs.")))

(defmethod drei-core:activate-target-specification ((spec group-target-specification))
  (ensure-group-views (group spec))
  (setf (drei-core:views spec) (group-views (group spec)))
  (call-next-method))

(defmethod drei-core:next-target :around ((spec group-target-specification))
  (handler-bind ((climacs1-gui:view-already-displayed
                  #'(lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'climacs1-gui:remove-other-use))))
    (call-next-method)))

(defmethod drei-core:previous-target :around ((spec group-target-specification))
  (handler-bind ((climacs1-gui:view-already-displayed
                  #'(lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'climacs1-gui:remove-other-use))))
    (call-next-method)))

(setf climacs1-gui:*climacs-target-creator*
      #'(lambda (drei)
          (make-instance 'group-target-specification
           :group (get-active-group)
           :drei-instance drei)))
