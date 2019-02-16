;;;; quadtree.lisp 
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

(defclass quadtree-node ()
  ((point :initarg :point :type vec2)
   (data :initarg :value :type t)))


(defclass quadtree ()
  ((point :type vec2)
   (depth :initform 0 :type fixnum)
   (data :type (or null cons))
   (top-left :type (or null quadtree))
   (top-right :type (or null quadtree))
   (bottom-left :type (or null quadtree))
   (bottom-right :type (or null quadtree)))
  (:documentation "A QuadTree class."))

(defgeneric insert (qt point new-item)
  (:documentation "Inserts item into qt at point.  Duplicates are allowed."))

(defmethod insert ((qt quadtree) new-point new-item)
  (if (not (slot-boundp qt 'point))
      (progn
        ;; (format t "No point in root!~%")
        (incf (slot-value qt 'depth))
        (setf (slot-value qt 'point) new-point)
        (setf (slot-value qt 'data) (list new-item)))
      (with-slots (point data depth) qt
        (format t "Point was bound...~%")
        (incf depth)
        (cond ((null point)
               ;; (format t "Point was null? How~%")
               (setf point new-point)
               (push new-item data))

              ;; This point
              ((v= point new-point)
               ;; (format t "Point was already present, pushing~%")
               (push new-item data))

              ;; Top left
              ((and (<= (vx new-point) (vx point))
                    (> (vy new-point) (vy point)))
               ;; (format t "Point was is above and left~%")
               (when (not (slot-boundp qt 'top-left))
                 (setf (slot-value qt 'top-left) (make-instance 'quadtree)))
               (insert (slot-value qt 'top-left) new-point new-item))

              ;; Bottom left
              ((and (<= (vx new-point) (vx point))
                    (<= (vy new-point) (vy point)))
               ;; (format t "Point was is below and left~%")
               (when (not (slot-boundp qt 'bottom-left))
                 (setf (slot-value qt 'bottom-left) (make-instance 'quadtree)))
               (insert (slot-value qt 'bottom-left) new-point new-item))

              ;; Top right
              ((and (> (vx new-point) (vx point))
                    (> (vy new-point) (vy point)))
               ;; (format t "Point was is above and right~%")
               (when (not (slot-boundp qt 'top-right))
                 (setf (slot-value qt 'top-right) (make-instance 'quadtree)))
               (insert (slot-value qt 'top-right) new-point new-item))

              ;; Bottom right
              ((and (> (vx new-point) (vx point))
                    (<= (vy new-point) (vy point)))
               ;; (format t "Point was is below and right~%")
               (when (not (slot-boundp qt 'bottom-right))
                 (setf (slot-value qt 'bottom-right) (make-instance 'quadtree)))
               (insert (slot-value qt 'bottom-right) new-point new-item))))))

(defgeneric qsize (qt)
  (:documentation "Returns the number of points in the quadtree."))

(defmethod qsize ((qt quadtree))
  (slot-value qt 'depth))

(defgeneric locate (qt item test)
  (:documentation "Returns nil if the item is not in the quadtree, returns the item's location otherwise."))

(defgeneric closest (qt point)
  (:documentation "Returns the point and value closest to point, returns nil if the quadtree is empty."))

(defgeneric remove-item (qt item test)
  (:documentation "Remove item from the quadtree."))

(defgeneric remove-from (qt point)
  (:documentation "Remove item from quadtree at point, if it exists."))
