;;;; point-quadtree.lisp 
;;
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
  ()
  (:documentation "A point quadtree, where space is subdivided at each point."))

(defmethod insert ((qt point-quadtree) new-point new-item)
  (with-slots (point data size) qt
    (incf size)
    (cond
      ((null point)
       (setf point new-point)
       (push new-item data))

      ((v= point new-point)
       (push new-item data))

      (t
       (let ((quad (quadrant-of point new-point)))
         (when (null (slot-value qt quad))
           (setf (slot-value qt quad) (make-instance 'point-quadtree)))
         (insert (slot-value qt quad) new-point new-item))))))

(defmethod qsize ((qt point-quadtree))
  (slot-value qt 'size))

(defmethod depth-first ((qt point-quadtree) function)
  (with-slots (point data size top-left top-right bottom-left bottom-right) qt
    (when top-left
      (depth-first top-left function))
    (when top-right
      (depth-first top-right function))
    (when bottom-right
      (depth-first bottom-right function))
    (when bottom-left
      (depth-first bottom-left function))
    (when (and point data)
      (funcall function qt))))

(defmethod locate ((qt point-quadtree) the-item test)
  (let ((results nil))
    (depth-first
     qt
     (lambda (node)
       (when (find the-item (slot-value node 'data) :test test)
         (push (slot-value node 'point) results))))
    results))

(defmethod closest ((qt point-quadtree) the-point)
  (error "Not implemented."))

(defun in-range-p (pt min-x max-x min-y max-y)
  (and (<= (vx pt) max-x)
       (<= (vy pt) max-y)
       (>= (vx pt) min-x)
       (>= (vy pt) min-y)))

(defmethod range-find ((qt quadtree) search-point range)
  (let ((min-x (- (vx search-point) range))
        (max-x (+ (vx search-point) range))
        (min-y (- (vy search-point) range))
        (max-y (+ (vy search-point) range)))
    (labels ((rfind (qt)
               (with-slots (point data size) qt
                 (let* ((quadrants (mapcar (curry #'quadrant-of point)
                                           (list (vec2 min-x max-y)
                                                 (vec2 max-x max-y)
                                                 (vec2 min-x min-y)
                                                 (vec2 max-x min-y))))
                        (unique-quads (remove-duplicates quadrants :test #'equal))
                        (rvals (loop
                                  for quad in unique-quads
                                  when (slot-value qt quad)
                                  appending (rfind (slot-value qt quad)))))
;;                   (format t "rvals: ~a~%" rvals)
                   (when (in-range-p point min-x max-x min-y max-y)
                     (push (cons point data) rvals))
                   rvals))))
      (rfind qt))))

(defmethod remove-item ((qt point-quadtree) item test)
  (error "Not implemented."))

(defmethod remove-from ((qt point-quadtree) point)
  (error "Not implemented."))
