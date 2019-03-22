;; point-quadtree.lisp 

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

(defclass point-quadtree (quadtree)
  ((entry :initform nil :type (or null quadtree-entry)))
  (:documentation "A point quadtree, where space is subdivided at each point."))

(defmethod insert ((qt point-quadtree) new-point new-item)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-slots (entry size) qt
    (incf size)
    (cond ((null entry)
           (setf entry (make-entry new-point new-item)))

          ((is-point entry new-point)
           (add-value entry new-item))

          (t
           (let ((quad (quadrant-of (slot-value entry 'point) new-point)))
             (when (null (slot-value qt quad))
               (setf (slot-value qt quad) (make-instance 'point-quadtree)))
             (insert (slot-value qt quad) new-point new-item))))))

(defmethod depth-first ((qt point-quadtree) function)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-slots (entry size top-left top-right bottom-left bottom-right) qt
    (when top-left
      (depth-first top-left function))
    (when top-right
      (depth-first top-right function))
    (when bottom-right
      (depth-first bottom-right function))
    (when bottom-left
      (depth-first bottom-left function))
    (when entry
      (funcall function entry))))

(defmethod locate ((qt point-quadtree) the-item test)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((results nil))
    (depth-first
     qt
     (lambda (entry)
       (when (contains entry the-item test)
         (push (slot-value entry 'point) results))))
    results))

(defmethod closest ((qt point-quadtree) the-point)
  (error "Not implemented."))

(defmethod range-find ((qt point-quadtree) search-point range)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((bounds (from-point-range search-point range)))
    (labels
        ((rfind (qt)
           (with-slots (entry size) qt
             (let* ((quadrants (mapcar (curry #'quadrant-of (slot-value entry 'point))
                                       (bounds-to-points bounds)))
                    (unique-quads (remove-duplicates quadrants :test #'equal))
                    (rvals (loop
                              for quad in unique-quads
                              when (slot-value qt quad)
                              append (rfind (slot-value qt quad)))))
               (if (inside-p (slot-value entry 'point) bounds)
                 (cons entry rvals)
                 rvals)))))
      (rfind qt))))

(defmethod remove-item ((qt point-quadtree) item test)
  (error "Not implemented."))

(defmethod remove-from ((qt point-quadtree) point)
  (error "Not implemented."))
