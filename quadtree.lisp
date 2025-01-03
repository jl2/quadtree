;; quadtree.lisp

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

(defparameter *top-left* 0)
(defparameter *top-right* 1)
(defparameter *bottom-left* 2)
(defparameter *bottom-right* 3)

(defclass quadtree ()
  ((size :initform 0 :type fixnum)
   (children :initform (make-array 4 :element-type '(or null quadtree::quadtree) :initial-element nil) :type (simple-vector 4))
   ;; (top-left :initform nil :type (or null quadtree))
   ;; (top-right :initform nil :type (or null quadtree))
   ;; (bottom-left :initform nil :type (or null quadtree))
   ;; (bottom-right :initform nil :type (or null quadtree))
   )
  (:documentation "A QuadTree class."))

(defgeneric qsize (qt)
  (:documentation "Returns the number of points in the quadtree."))

(declaim (inline locate depth-first closest range-find remove-item remove-from qsize))
(defgeneric insert (qt point new-item)
  (:documentation "Inserts item into qt at point.  Duplicates are allowed."))

(defgeneric locate (qt item test)
  (:documentation "Returns nil if the item is not in the quadtree, returns the item's location otherwise."))

(defgeneric depth-first (qt function)
  (:documentation "Depth first traversal of quadtree."))

(defgeneric closest (qt point)
  (:documentation "Returns the point and value closest to point, returns nil if the quadtree is empty."))

(defgeneric range-find (qt point range)
  (:documentation "Returns the point and value closest to point, returns nil if the quadtree is empty."))

(defgeneric remove-item (qt item test)
  (:documentation "Remove item from the quadtree."))

(defgeneric remove-from (qt point)
  (:documentation "Remove item from quadtree at point, if it exists."))

(defmethod qsize (qt)
  (slot-value qt 'size))

(defgeneric view-quadtree (qt
                           png-file-name
                           &key width height open-png x-scale y-scale line-width)
  (:documentation "Create PNG representation of the quadtree, saving to the file png-file-name."))

(defun home-dir (path)
  "Utility function to make relative path names relative to the user's home directory to work around Cairo weirdness."
  (merge-pathnames path (user-homedir-pathname)))

(declaim (inline quadrant-of))
(defun quadrant-of (root pt)
  "Returns the quadrant of pt relative to root."
  (cond ((and (< (vx pt) (vx root))
              (>= (vy pt) (vy root)))
         *top-left*)
        ((and (< (vx pt) (vx root))
              (< (vy pt) (vy root)))
         *bottom-left*)
        ((and (>= (vx pt) (vx root))
              (>= (vy pt) (vy root)))
         *top-right*)
        ((and (>= (vx pt) (vx root))
              (< (vy pt) (vy root)))
         *bottom-right*)))

(defun opposite-quadrant (quad)
  "Returns the quadrant 180 degrees from quad."
  (cond ((eq quad *top-left*)
         *bottom-right*)
        ((eq quad *top-right*)
         *bottom-left*)
        ((eq quad *bottom-left*)
         *top-right*)
        ((eq quad *bottom-right*)
         *top-left*)
        (t (error "Unknown quadrant! ~a" quad))))
