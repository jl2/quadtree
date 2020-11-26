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
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-slots (entries) qt
    (> (length entries) *split-size*)))

(defun split-quadtree (qt)
  "Split a quadtree into 4 new nodes."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-slots (bounds children entries size) qt
    (loop for this-bound across (split-bounds bounds)
       for quad-idx from 0
       do
         (setf (aref children quad-idx) (make-instance 'pr-quadtree :bounds this-bound))
         (dolist (entry entries)
           (when (inside-p (slot-value entry 'point) this-bound)
             (incf (slot-value (aref children quad-idx) 'size))
             (push entry (slot-value (aref children quad-idx) 'entries))))
         (when (needs-split (aref children quad-idx))
           (split-quadtree (aref children quad-idx))))
    (setf entries nil)))

(defmethod insert ((qt pr-quadtree) new-point new-item)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-slots (bounds children entries size ) qt
    (when (not (inside-p new-point bounds))
      (error "~a is not inside quadtree bounds." new-point))
    (cond
      ;; Empty tree
      ((every #'null (list entries (aref children *top-left*) (aref children *top-right*) (aref children *bottom-left*) (aref children *bottom-right*)))
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
         (insert (aref children quad) new-point new-item))))
    (incf size)))


(defmethod range-find ((qt pr-quadtree) search-point range)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((find-bound (from-point-range search-point range)))
    (labels
        ((rfind (qt)
           (with-slots (entries children bounds size) qt
             (if (null entries)
                 ;; Recursion case
                 ;; Entries is null, so all data is in subtrees
                 ;; For each quadtree, if it's qsize > 0 then 
                 (loop for quad in (list *top-left* *bottom-left* *top-right* *bottom-right*)
                    for sub-tree = (aref children quad) then (aref children quad)
                    when (and sub-tree (not (zerop (qsize sub-tree))))
                    append (rfind sub-tree))
                 ;; End recursion case
                 ;; All data is in entries, so check whether each point is in find-bound
                 (remove-if-not (rcurry #'inside-p find-bound)
                                entries
                                :key
                                (lambda (x) (slot-value x 'point)))))))
      (rfind qt))))



