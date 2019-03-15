;; pr-quadtree.lisp

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

(defparameter *split-size* 8)

(defclass pr-quadtree (quadtree)
  ((entries :initform nil :type (or null cons))
   (bounds :initarg :bounds :initform (make-instance 'quadtree-bounds):type quadtree-bounds))
  (:documentation "A point-range quadtree, where space is subdivided into four equal parts at each level of the tree."))

(declaim (inline needs-split))
(defun needs-split (qt)
  "Returns true when a quadtree has more than *split-size* entries."
  (with-slots (entries) qt
    (> (length entries) *split-size*)))

(defun split-quadtree (qt)
  "Split a quadtree into 4 new nodes."
  (with-slots (bounds entries size) qt
    (dolist (new-bound (split-bounds bounds))
      (let ((quad-name (car new-bound))
            (this-bound (cdr new-bound)))
        (setf (slot-value qt quad-name) (make-instance 'pr-quadtree :bounds this-bound))
        (dolist (entry entries)
          (when (inside-p (slot-value entry 'point) this-bound)
            (incf (slot-value (slot-value qt quad-name) 'size))
            (push entry (slot-value (slot-value qt quad-name) 'entries))))
        (when (needs-split (slot-value qt quad-name))
          (split-quadtree (slot-value qt quad-name)))))
    (setf entries nil)))

(defmethod insert ((qt pr-quadtree) new-point new-item)
  (with-slots (bounds entries size top-left top-right bottom-left bottom-right) qt
    (when (not (inside-p new-point bounds))
      (error "~a is not inside quadtree bounds." new-point))
    (cond
      ;; Empty tree
      ((every #'null (list entries top-left top-right bottom-left bottom-right))
       (setf entries (list (make-entry new-point new-item))))

      ;; Not empty but smaller than *split-size*
      ((and (not (null entries)) (not (needs-split qt)))
       (let ((existing-entry (find-if (rcurry #'is-point new-point) entries)))
         (cond (existing-entry
                (add-value existing-entry new-item))
               (t
                (push (make-entry new-point new-item) entries)
                (when (needs-split qt)
                  (split-quadtree qt))))))
      (t
       (let ((quad (quadrant-of (midpoint bounds) new-point)))
         (insert (slot-value qt quad) new-point new-item))))
    (incf size)))


(defmethod range-find ((qt pr-quadtree) search-point range)
  (let ((find-bound (from-point-range search-point range)))
    (labels
        ((rfind (qt)
           (with-slots (entries bounds size) qt
             (if (null entries)
                 ;; Recursion case
                 ;; Entries is null, so all data is in subtrees
                 ;; For each quadtree, if it's qsize > 0 then 
                 (loop for quad in '(top-left bottom-left top-right bottom-right)
                    for sub-tree = (slot-value qt quad) then (slot-value qt quad)
                    when (and sub-tree (not (zerop (qsize sub-tree))))
                    append (rfind sub-tree))
                 ;; End recursion case
                 ;; All data is in entries, so check whether each point is in find-bound
                 (remove-if-not (rcurry #'inside-p find-bound)
                                entries
                                :key
                                (lambda (x) (slot-value x 'point)))))))
      (rfind qt))))



