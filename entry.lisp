;; entry.lisp 

;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :quadtree)

(defclass quadtree-entry ()
  ((point :initarg :point :type vec2)
   (data :initarg :data :type (or null cons))))

(defmethod print-object ((entry quadtree-entry) stream)
  (with-slots (point data) entry
    (format stream "(make-quadtree-entry :point ~a :data '(~{~a~^ ~}))" point data)))
    
(declaim (inline make-entry add-value remove-value is-point contains))
(defun make-entry (point data)
  "Create a new quadtree entry."
  (make-instance 'quadtree-entry :point point :data (list data)))

(defun add-value (entry new-data)
  "Add a new value to a quadtree entry."
  (push new-data (slot-value entry 'data)))

(defun remove-value (entry value &optional (test #'equal))
  "Remove a value from a quadtree entry, if it exists."
  (setf (slot-value entry 'data) (remove-if (curry test value) (slot-value entry 'data))))

(defun is-point (entry point)
  "Check if entry's point is equal to point."
  (v= (slot-value entry 'point) point))

(defun contains (entry value &optional (test #'equal))
  "Check if value is in entry."
  (find value (slot-value entry 'data) :test test))

(defun from-existing (entry)
  (make-instance 'quadtree-entry :point (slot-value entry 'point) :data (slot-value entry 'data)))
