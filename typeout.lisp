;;;  (c) copyright 2008 by
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

;;; Typeout is the word for "output whatever". It's a facility for
;;; drawing stuff that doesn't go in a buffer. There are two kinds of
;;; typeout: typeout views, that act like uneditable views, and
;;; typeout overlays, that are highly ephemeral and temporary
;;; creations to be used for short-lived interaction.

(cl:in-package #:climacs1-gui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Typeout views.

(defclass typeout-view (drei:drei-view clim:textual-view)
  ((%output-history
    :accessor output-history
    :initform (make-instance 'clim:standard-tree-output-record)
    :initarg :output-history
    :documentation
    #.(format nil "The output record history that will be replayed~@
                   whenever the views contents are shown."))
   (%dirty
    :accessor dirty
    :initform t
    :documentation
    #.(format nil "This value indicates whether the output has~@
                   changed since it was last replayed."))
   (%cursor-position
    :accessor last-cursor-position
    :initform nil
    :documentation
    #.(format nil "A list (X Y) specifying where drawing ended the last ~@
                   time, and where it should start the next time. If NIL,~@
                   no previous position has been recorded.")))
  (:metaclass esa-utils:modual-class)
  (:documentation #.(format nil "A noneditable Drei view displaying an~@
                                 output record history.")))

(defun typeout-view-p (view)
  "Return true if `view' is a typeout view, false otherwise."
  (typep view 'typeout-view))

(defmethod drei:clear-redisplay-information ((view typeout-view))
  (setf (dirty view) t))

(defun blank-typeout-view (view)
  "Blank out the contents of the typeout view `view'."
  (setf (output-history view) (make-instance 'clim:standard-tree-output-record)
        (last-cursor-position view) nil)
  (drei:clear-redisplay-information view)
  ;; If it's on display, clear the window too.
  (let ((window (find view (esa:windows clim:*application-frame*)
                 :key #'clim:view)))
    (when window (clim:window-clear window))))

(defmethod drei:handle-redisplay
    ((pane drei:drei-pane) (view typeout-view) (region clim:region))
  (if (and (not (dirty view))
           (eq (clim:output-record-parent (output-history view))
               (clim:stream-output-history pane)))
      (unless (clim:region-equal region clim:+nowhere+)
        (let ((region (if (clim:region-equal region clim:+everywhere+)
                          (clim:sheet-region pane)
                          (clim:bounding-rectangle region))))
          (clim:with-bounding-rectangle* (x1 y1 x2 y2) region
            (clim:with-output-recording-options (pane :record nil)
              (clim:draw-rectangle* pane x1 y1 x2 y2
                               :filled t :ink clim:+background-ink+)))
          (clim:replay (clim:stream-output-history pane) pane region)))
      (call-next-method)))

(defmethod drei:display-drei-view-contents
    ((pane clim:pane) (view typeout-view))
  (when (or (dirty view)
            (not (eq (clim:output-record-parent (output-history view))
                     (clim:stream-output-history pane))))
    (clim:with-output-recording-options (pane :record nil :draw t)
      (clim:with-bounding-rectangle* (x1 y1 x2 y2)
                                     (or (clim:pane-viewport-region pane)
                                         (clim:sheet-region pane))
        (clim:draw-rectangle* pane x1 y1 x2 y2 :ink clim:+background-ink+))
      (clim:replay-output-record (output-history view) pane
                                 (or (clim:pane-viewport-region pane)
                                     (clim:sheet-region pane))))
    (unless (eq (clim:output-record-parent (output-history view))
                (clim:stream-output-history pane))
      (setf (clim:output-record-parent (output-history view)) nil)
      (clim:add-output-record (output-history view)
                              (clim:stream-output-history pane))))
  (setf (dirty view) nil))

(defmethod clim:bounding-rectangle* ((view typeout-view))
  (if (output-history view)
      (clim:bounding-rectangle* (output-history view))
      (values 0 0 0 0)))

(defun scroll-typeout-window (window y)
  #.(format nil "Scroll WINDOW down by `y' device units, but taking~@
                 care not to scroll past the size of WINDOW. If WINDOW~@
                 does not have a viewport, do nothing.")
  (let ((viewport (clim:pane-viewport window)))
    (unless (null viewport)            ; Can't scroll without viewport
      (multiple-value-bind (x-displacement y-displacement)
          (clim:transform-position (clim:sheet-transformation window) 0 0)
        (clim:scroll-extent
         window
         (- x-displacement)
         (max 0 (min (+ (- y-displacement) y)
                     (- (clim:bounding-rectangle-height window)
                        (clim:bounding-rectangle-height viewport)))))))))

(defmethod drei:page-down ((pane clim:sheet) (view typeout-view))
  (let* ((viewport (clim:pane-viewport pane))
         (height (clim:bounding-rectangle-height viewport)))
    (scroll-typeout-window pane height)))

(defmethod drei:page-up ((pane clim:sheet) (view typeout-view))
  (scroll-typeout-window
   pane (- (clim:bounding-rectangle-height (clim:pane-viewport pane)))))

(defun ensure-typeout-view (climacs label erase)
  #.(format nil "Ensure that CLIMACS has a typeout view with the name~@
                 LABEL, and return that view. If ERASE is true, clear any~@
                 already existing typeout view by that name first.")
  (check-type label string)
  (or (let ((view (find-if #'(lambda (view)
                               (and (typeout-view-p view)
                                    (string= (esa-utils:name view) label)))
                           (drei-core:views climacs))))
        (when (and view erase) (blank-typeout-view view))
        view)
      (make-new-view-for-climacs climacs 'typeout-view
       :name label)))

;; Because specialising on the type of `climacs' is so useful...
(defun invoke-with-typeout-view (climacs label erase continuation)
  #.(format nil "Call CONTINUATION with a single argument, a stream~@
                 meant for typeout. CLIMACS is the Climacs instance in~@
                 which the typeout pane should be shown, and LABEL is~@
                 the name of the created typeout view. Returns NIL.")
  (let* ((typeout-view (ensure-typeout-view climacs label erase))
         (pane-with-typeout-view (or (find typeout-view (esa:windows climacs)
                                      :key #'clim:view)
                                     (let ((pane (split-window t)))
                                       (setf (clim:view pane) typeout-view)
                                       pane))))
    (let ((new-record
            (clim:with-output-to-output-record (pane-with-typeout-view)
              (clim:with-output-recording-options (pane-with-typeout-view :record t :draw nil)
                (when (last-cursor-position typeout-view)
                  (setf (clim:stream-cursor-position pane-with-typeout-view)
                        (values-list (last-cursor-position typeout-view))))
                (funcall continuation pane-with-typeout-view)
                (setf (last-cursor-position typeout-view)
                      (multiple-value-list (clim:stream-cursor-position pane-with-typeout-view)))))))
      (clim:add-output-record new-record (output-history typeout-view))
      (setf (dirty typeout-view) t)
      nil)))

(defmacro with-typeout-view ((stream &optional (label "Typeout") erase)
                             &body body)
  #.(format nil "Evaluate BODY with STREAM bound to a stream that can be~@
                 used for typeout. LABEL is the name of the created typeout~@
                 view. If ERASE is true, clear the contents of any existing~@
                 typeout view with that name.")
  `(invoke-with-typeout-view esa:*esa-instance* ,label ,erase
                             #'(lambda (,stream)
                                 ,@body)))

;;; An implementation of the Gray streams protocol that uses a Climacs
;;; typeout view to draw the output.

(defclass typeout-stream
    (trivial-gray-streams:fundamental-character-output-stream)
  ((%climacs :reader climacs-instance
             :initform (error "Must provide a Climacs instance for typeout streams")
             :initarg :climacs)
   (%label :reader label
           :initform (error "A typeout stream must have a label")
           :initarg :label))
  (:documentation
   #.(format nil "An output stream that performs output on a (single)~@
                  Climacs typeout pane. If the typeout pane is deleted~@
                  manually by the user, the stream will recreate it the~@
                  next time output is performed.")))

(defmethod trivial-gray-streams:stream-write-char ((stream typeout-stream) char)
  (with-typeout-view (typeout (label stream))
    (trivial-gray-streams:stream-write-char typeout char)))

(defmethod trivial-gray-streams:stream-line-column ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (trivial-gray-streams:stream-line-column typeout)))

(defmethod trivial-gray-streams:stream-start-line-p ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (trivial-gray-streams:stream-start-line-p typeout)))

(defmethod trivial-gray-streams:stream-write-string ((stream typeout-stream) string &optional (start 0) end)
  (with-typeout-view (typeout (label stream))
    (trivial-gray-streams:stream-write-string typeout string start end)))

(defmethod trivial-gray-streams:stream-terpri ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (trivial-gray-streams:stream-terpri typeout)))

(defmethod trivial-gray-streams:stream-fresh-line ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (trivial-gray-streams:stream-fresh-line typeout)))

(defmethod trivial-gray-streams:stream-finish-output ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (trivial-gray-streams:stream-finish-output typeout)))

(defmethod trivial-gray-streams:stream-force-output ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (trivial-gray-streams:stream-force-output typeout)))

(defmethod trivial-gray-streams:stream-clear-output ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (trivial-gray-streams:stream-clear-output typeout)))

(defmethod trivial-gray-streams:stream-advance-to-column ((stream typeout-stream) (column integer))
  (with-typeout-view (typeout (label stream))
    (trivial-gray-streams:stream-advance-to-column typeout column)))

(defmethod clim-lisp-patch:interactive-stream-p ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (clim-lisp-patch:interactive-stream-p typeout)))

(defun make-typeout-stream (climacs label)
  (make-instance 'typeout-stream :climacs climacs :label label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Typeout overlays.

(defclass overlaying-pane (clim:bboard-pane)
  ((%overlay-pane :accessor overlay-pane
                  :initform nil
                  :type (or null clim:pane)
                  :documentation
                  #.(format nil "The overlay pane. When this is set,~@
                                 the overlay tree will be updated."))
   (%overlay-tree :accessor overlay-tree
                  :initform nil
                  :type (or null clim:pane)
                  :documentation
                  #.(format nil "The pane hierarchy containing the  overlay~@
                                 pane. Should not be changed manually, will~@
                                 be updated when the overlay-pane is set."))
   (%content-pane :reader content-pane
                  :initform (error "A content-pane must be provided")
                  :type clim:pane
                  :initarg :contents
                  :documentation
                  #.(format nil "The pane containing the usually~@
                                 displayed contents.")))
  (:documentation
   #.(format nil "This layout pane facilitates the addition and removal~@
                  of an overlay pane positioned at the top of the~@
                  OVERLAYING-PANE that will obscure the contents.~@
                  For ease of use, the overlay-pane and the pane hierarchy~@
                  containing this pane are handled seperately.")))

(defun find-topmost-parent (sheet)
  #.(format nil "Find the topmost parent of SHEET, that is the parent of~@
                 SHEET (or SHEET itself) that does not have a sheet parent~@
                 or has a graft parent.")
  (if (or (not (clim:sheetp (clim:sheet-parent sheet)))
          (typep (clim:sheet-parent sheet) 'clim:graft))
      sheet
      (find-topmost-parent (clim:sheet-parent sheet))))

(defmethod (setf overlay-pane) :before (new-overlay (pane overlaying-pane))
  (when (overlay-pane pane)
    (clim:sheet-disown-child pane (overlay-tree pane))
    (setf (overlay-tree pane) nil)))

(defmethod (setf overlay-pane) :after (new-overlay (pane overlaying-pane))
  (when new-overlay
    (let ((topmost-parent (find-topmost-parent new-overlay)))
      (clim:sheet-adopt-child pane topmost-parent)
      (setf (overlay-tree pane) topmost-parent))))

(defmethod initialize-instance :after ((object overlaying-pane) &rest args
                                       &key overlay)
  (declare (ignore args))
  (when overlay
    (setf (overlay-pane object) overlay))
  (clim:sheet-adopt-child object (content-pane object)))

(defmethod clim:allocate-space ((pane overlaying-pane) width height)
  (clim:allocate-space (content-pane pane) width height)
  (with-accessors ((overlay overlay-tree)) pane
    (when overlay
      (clim:move-sheet overlay 0 0)
      (clim:allocate-space
       overlay width (clim:space-requirement-height (clim:compose-space overlay))))))

(defmethod clim:compose-space ((pane overlaying-pane) &rest args)
  (apply #'clim:compose-space (content-pane pane) args))

(defmacro overlaying ((&rest options) &body contents)
  #.(format nil "Create an overlaying pane with CONTENTS arranged~@
                 vertically as the contents of the overlaying pane.~@
                 There will be no initial overlay.")
  `(clim:make-pane 'overlaying-pane ,@options :contents (clim:vertically () ,@contents)))

(defun pane-overlayer (pane)
  "Return the `overlaying-pane' that contains `pane'"
  (if (typep pane 'overlaying-pane)
      pane
      (unless (null (clim:sheet-parent pane))
        (pane-overlayer (clim:sheet-parent pane)))))

(defun add-typeout (&optional (pane (esa:current-window)))
  #.(format nil "Return the typeout overlay of PANE,~@
                 creating one if it doesn't exist.")
  (clim:with-look-and-feel-realization
      ((clim:frame-manager (clim:pane-frame pane)) (clim:pane-frame pane))
    (let ((overlayer (pane-overlayer pane)))
      (or (overlay-pane overlayer)
          (let ((overlay (clim:make-pane 'typeout-overlay
                          :width (clim:bounding-rectangle-width
                                  (clim:sheet-region overlayer)))))
            (clim:outlining () overlay) ; This adds an outlining-pane as
                                   ; the parent of `overlay'.
            (setf (overlay-pane overlayer) overlay))))))

(defun remove-typeout (&optional (pane (esa:current-window)))
  #.(format nil "Remove the typeout overlay of PANE, defaulting to~@
                 the current window.")
  (setf (overlay-pane (pane-overlayer pane)) nil))

(defclass typeout-overlay (clim:clim-stream-pane)
  ()
  (:default-initargs :background clim:+cornsilk1+
                     :scroll-bars nil))

(defun invoke-with-typeout (pane continuation &key erase)
  #.(format nil "Invoke CONTINUATION with a single argument - a typeout~@
                 overlay for PANE. If ERASE is true, the typeout~@
                 overlay will be newly created, and any old overlay will~@
                 have been deleted.")
  (clim:with-look-and-feel-realization
      ((clim:frame-manager (clim:pane-frame pane)) (clim:pane-frame pane))
    (when erase (remove-typeout pane))
    (let* ((typeout (add-typeout pane)))
      ;; Expand the typeout to the proper width...
      (clim:change-space-requirements typeout)
      (let ((values (multiple-value-list
                     (funcall continuation typeout))))
        (remove-typeout pane)
        (values-list values)))))

(defmacro with-typeout
    ((stream &rest args &key erase (window (esa:current-window)))
     &body body)
  #.(format nil "Evaluate BODY with STREAM bound to a typeout~@
                 overlay for WINDOW. If ERASE is true, the~@
                 typeout overlay will be newly created, and any~@
                 old overlay will have been deleted.")
  (declare (ignore erase))
  (esa-utils:with-keywords-removed (args (:window))
    `(invoke-with-typeout ,window
                          #'(lambda (,stream)
                              ,@body)
                          ,@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A frame manager for using typeout when appropriate.

(defclass climacs-frame-manager (clim:frame-manager)
  ((%standard-frame-manager
    :reader standard-frame-manager
    :initform (clim:find-frame-manager)
    :type clim:frame-manager
    :documentation
    #.(format nil "The frame manager that this CLIMACS-FRAME-MANAGER~@
                   dispatches functions to.")
    :initarg :standard-frame-manager))
  (:documentation
   #.(format nil "This class thinly wraps another frame manager instance~@
                  and delegates most frame managing to this other manager.~@
                  It is used to implement Climacs \"look and feel\" where~@
                  appropriate.")))

;;; A simple dispatching implementation of the frame manager protocol.

(defmacro define-dispatching-fun (name (frame-manager-arg &rest args))
  #.(format nil "Defines a dispatching function for the frame manager~@
                 protocol for CLIMACS-FRAME-MANAGER. Will assume that~@
                 FRAME-MANAGER-ARG is the frame manager.")
  `(defmethod ,name ((,frame-manager-arg climacs-frame-manager) ,@args)
     (when (standard-frame-manager ,frame-manager-arg)
       (,name (standard-frame-manager ,frame-manager-arg) ,@args))))

(define-dispatching-fun clim:frame-manager-frames (frame-manager))
(define-dispatching-fun clim:adopt-frame (frame-manager frame))
(define-dispatching-fun clim:disown-frame (frame-manager frame))
(define-dispatching-fun clim:port (frame-manager))
(define-dispatching-fun clim:note-frame-enabled (frame-manager frame))
(define-dispatching-fun clim:note-frame-disabled (frame-manager frame))
(define-dispatching-fun clim:note-frame-iconified (frame-manager frame))
(define-dispatching-fun clim:note-frame-deiconified (frame-manager frame))
(define-dispatching-fun clim:note-command-enabled (frame-manager frame command-name))
(define-dispatching-fun clim:note-command-disabled (frame-manager frame command-name))

(defmethod clim:frame-manager-notify-user ((frame-manager climacs-frame-manager) message &rest args)
  (apply #'clim:frame-manager-notify-user frame-manager message args))

(define-dispatching-fun clim:generate-panes (frame-manager frame))
(define-dispatching-fun clim:find-pane-for-frame (frame-manager frame))

;;; Now for the look & feel.

(defun menu-item-option (menu-item option &optional default)
  (if (listp menu-item)
      (getf (clim:menu-item-options menu-item) option default)
      default))

(clim:define-presentation-type typeout-menu-item ())

(defmethod clim:menu-choose-from-drawer
    ((menu typeout-overlay) presentation-type drawer
     &key x-position y-position cache unique-id id-test cache-value cache-test
     default-presentation pointer-documentation)
  (declare (ignore cache unique-id
                   id-test cache-value cache-test default-presentation
                   x-position y-position))
  (clim:with-room-for-graphics (menu :first-quadrant nil)
    (funcall drawer menu presentation-type))
  (let ((clim:*pointer-documentation-output* pointer-documentation))
    (handler-case
        (clim:with-input-context (`(or ,presentation-type clim:blank-area) :override t)
            (object type clim:event)
            (prog1 nil (loop for gesture = (clim:read-gesture :stream menu :peek-p t)
                             until (or (and (typep gesture 'clim:keyboard-event)
                                            (clim:keyboard-event-character gesture))
                                       (characterp gesture))
                             do (clim:read-gesture :stream menu)))
          (clim:blank-area nil)
          (t (values object event)))
      (clim:abort-gesture () nil))))

(defmethod clim:frame-manager-menu-choose
    ((frame-manager climacs-frame-manager) items
     &rest options
     &key (associated-window (esa:current-window))
       printer
       presentation-type
       (default-item nil default-item-p)
       text-style label cache unique-id id-test cache-value cache-test
       max-width max-height n-rows n-columns x-spacing y-spacing row-wise
       cell-align-x cell-align-y (scroll-bars :vertical)
     ;; We provide pointer documentation by default.
       (pointer-documentation clim:*pointer-documentation-output*))
  (flet ((drawer (overlay type)
           (let* ((height (clim:bounding-rectangle-height
                           (clim:with-new-output-record (overlay)
                             (when label
                               (clim:with-text-style (overlay (clim:make-text-style :serif :italic :large))
                                 (write-string label overlay)
                                 (terpri overlay)))
                             (clim:draw-standard-menu overlay type items
                                                 (if default-item-p
                                                     default-item
                                                     (first items))
                                                 :item-printer (or printer
                                                                   #'clim:print-menu-item)
                                                 :max-width max-width
                                                 :max-height max-height
                                                 :n-rows n-rows
                                                 :n-columns n-columns
                                                 :x-spacing x-spacing
                                                 :y-spacing y-spacing
                                                 :row-wise row-wise
                                                 :cell-align-x cell-align-x
                                                 :cell-align-y cell-align-y))))
                  (overlayer (pane-overlayer overlay))
                  (overlay-tree (overlay-tree overlayer)))
             ;; Tell it how big it is.
             (clim:change-space-requirements overlay :height height)
             ;; Bigger than the available space? User OK with ugly?
             ;; Then add scrolling.
             (when (and (> height (clim:bounding-rectangle-height overlayer))
                        scroll-bars
                        (not (typep overlay-tree 'clim:scroller-pane)))
               (setf (overlay-pane overlayer) nil ; To clear the parent/child relationship
                     (overlay-pane overlayer)
                     (prog1 overlay
                       (clim:scrolling (:scroll-bars scroll-bars) ; Now re-add with scroll-bars.
                         overlay-tree)))
               ;; The overlayer has default space requirements now,
               ;; make it reevaluate its life.
               (clim:change-space-requirements overlayer)))))
    (multiple-value-bind (object event)
        (with-typeout (menu :erase t :window associated-window)
          (when text-style
            (setf (clim:medium-text-style menu) text-style))
          (esa-utils:letf (((clim:stream-default-view menu) clim:+textual-menu-view+))
            (clim:menu-choose-from-drawer menu (or presentation-type 'typeout-menu-item)
                                     #'drawer
                                     :cache cache
                                     :unique-id unique-id
                                     :id-test id-test
                                     :cache-value cache-value
                                     :cache-test cache-test
                                     :pointer-documentation pointer-documentation)))
      (unless (null event)             ; Event is NIL if user aborted.
        (let ((subitems (menu-item-option object :items 'menu-item-no-items)))
          (if (eq subitems 'menu-item-no-items)
              (values (clim:menu-item-value object) object event)
              (apply #'clim:frame-manager-menu-choose
                     frame-manager subitems
                     options)))))))
