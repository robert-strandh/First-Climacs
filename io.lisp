;;;  (c) copyright 2004, 2017 by
;;;           Robert Strandh (robert.strandh@gmail.com)
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

;;; Input/Output of buffers to and from streams.

(cl:in-package #:climacs-core)

(define-condition buffer-contains-noncharacter (esa-io:buffer-writing-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Buffer ~A contains non-character object"
                     (esa-utils:name (esa-io:buffer condition)))))
  (:documentation
   #.(format nil "This error is signalled whenever an attempt is made~@
                  to save a buffer that contains a non-character object.")))

(defun buffer-contains-noncharacter (buffer filepath)
  #.(format nil "Signal an error of type BUFFER-CONTAINS-NONCHARACTER~@
                 with the buffer BUFFER and the filepath FILEPATH.")
  (error 'buffer-contains-noncharacter :buffer buffer :filepath filepath))

(defmethod esa-io:check-buffer-writability
    ((application-frame climacs1-gui:climacs)
     (filepath pathname)
     (buffer drei:drei-buffer))
  (drei-base:do-buffer-region (object offset buffer 0 (drei-buffer:size buffer))
    (unless (characterp object)
      (buffer-contains-noncharacter buffer filepath)))
  (call-next-method))

(defmethod esa-buffer:frame-save-buffer-to-stream
    ((application-frame climacs1-gui:climacs)
     (buffer climacs1-gui:climacs-buffer)
     stream)
  (let ((seq (drei-buffer:buffer-sequence buffer 0 (drei-buffer:size buffer))))
    (if (every #'characterp seq)
        (write-sequence seq stream)
        (esa:display-message
         "Cannot save to file, buffer contains non-character object"))))

(defun input-from-stream (stream buffer offset)
  (let* ((seq (make-string (file-length stream)))
         (count (#+mcclim read-sequence #-mcclim cl:read-sequence
                 seq stream)))
    (drei-buffer:insert-buffer-sequence buffer offset
                                        (if (= count (length seq))
                                            seq
                                            (subseq seq 0 count)))))

(defmethod esa-buffer:frame-make-buffer-from-stream
    ((application-frame climacs1-gui:climacs) stream)
  (let* ((buffer (esa-buffer:make-new-buffer)))
    (input-from-stream stream buffer 0)
    (drei:clear-undo-history buffer)
    buffer))
