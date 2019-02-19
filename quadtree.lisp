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
  ((point :initform nil :type (or null vec2))
   (size :initform 0 :type fixnum)
   (data :initform nil :type (or null cons))
   (top-left :initform nil :type (or null quadtree))
   (top-right :initform nil :type (or null quadtree))
   (bottom-left :initform nil :type (or null quadtree))
   (bottom-right :initform nil :type (or null quadtree)))
  (:documentation "A QuadTree class."))

(defgeneric insert (qt point new-item)
  (:documentation "Inserts item into qt at point.  Duplicates are allowed."))

(defmethod insert ((qt quadtree) new-point new-item)
  (with-slots (point data size top-left top-right bottom-left bottom-right) qt
    (incf size)
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
           (when (null top-left)
             (setf top-left (make-instance 'quadtree)))
           (insert top-left new-point new-item))

          ;; Bottom left
          ((and (<= (vx new-point) (vx point))
                (<= (vy new-point) (vy point)))
           ;; (format t "Point was is below and left~%")
           (when (null bottom-left)
             (setf bottom-left (make-instance 'quadtree)))
           (insert bottom-left new-point new-item))

          ;; Top right
          ((and (> (vx new-point) (vx point))
                (> (vy new-point) (vy point)))
           ;; (format t "Point was is above and right~%")
           (when (null top-right)
             (setf top-right (make-instance 'quadtree)))
           (insert top-right new-point new-item))

          ;; Bottom right
          ((and (> (vx new-point) (vx point))
                (<= (vy new-point) (vy point)))
           ;; (format t "Point was is below and right~%")
           (when (null bottom-right)
             (setf bottom-right (make-instance 'quadtree))) 
          (insert bottom-right new-point new-item)))))

(defgeneric qsize (qt)
  (:documentation "Returns the number of points in the quadtree."))

(defmethod qsize ((qt quadtree))
  (slot-value qt 'size))

(defgeneric locate (qt item test)
  (:documentation "Returns nil if the item is not in the quadtree, returns the item's location otherwise."))

(defgeneric depth-first (qt function)
  (:documentation "Depth first traversal of quadtree."))

(defmethod depth-first ((qt quadtree) function)
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

(defmethod locate (qt the-item test)
  (let ((results nil))
    (depth-first
     qt
     (lambda (node)
       (when (find the-item (slot-value node 'data) :test test)
         (push (slot-value node 'point) results))))
    results))

(defgeneric closest (qt point)
  (:documentation "Returns the point and value closest to point, returns nil if the quadtree is empty."))

(defmethod closest (qt the-point)
  (with-slots (point data depth top-left top-right bottom-left bottom-right) qt
    (cond ((null point)
           (values nil nil))

          ((v= point the-point)
           (values point data))

          ;; Top left
          ((and (<= (vx the-point) (vx point))
                (> (vy the-point) (vy point)))
           (if (null top-left)
               (values point data)
               (closest top-left the-point)))

          ;; Bottom left
          ((and (<= (vx the-point) (vx point))
                (<= (vy the-point) (vy point)))
           (if (null bottom-left)
               (values point data)
               (closest bottom-left the-point)))

          ;; Top right
          ((and (> (vx the-point) (vx point))
                (> (vy the-point) (vy point)))
           (if (null top-right)
               (values point data)
               (closest top-right the-point)))

          ;; Bottom right
          ((and (> (vx the-point) (vx point))
                (<= (vy the-point) (vy point)))
           (if (null bottom-right)
               (values point data)
               (closest bottom-right the-point))))))

(defgeneric remove-item (qt item test)
  (:documentation "Remove item from the quadtree."))


(defgeneric remove-from (qt point)
  (:documentation "Remove item from quadtree at point, if it exists."))
