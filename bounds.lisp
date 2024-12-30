;; bounds.lisp 

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

(declaim (optimize (speed 1) (safety 3) (debug 3)))

(defclass quadtree-bounds ()
  ((x-min :initarg :x-min
          :initform -1000.0
          :type number)
   (x-max :initarg :x-max
          :initform 1000.0
          :type number)
   (y-min :initarg :y-min
          :initform -1000.0
          :type number)
   (y-max :initarg :y-max
          :initform 1000.0
          :type number))
  (:documentation "Quadtree node boundary"))

(defmethod print-object ((bound quadtree-bounds) stream)
  (declare (type stream stream))
  (with-slots (x-min y-min x-max y-max) bound
    (format stream
            "(make-bounds :x-min ~a :y-min ~a :x-max ~a :y-max ~a)"
            x-min y-min
            x-max y-max)))

(declaim (inline from-point-range inside-p bounds-to-points midpoint))
(defun from-point-range (point range)
  "Create a quadtree-bounds centered at point extending to ± range in each direction."
  (make-instance 'quadtree-bounds
                 :x-min (- (vx point) range)
                 :x-max (+ (vx point) range)
                 :y-min (- (vy point) range)
                 :y-max (+ (vy point) range)))

(defun inside-p (point bounds)
  "Check if point is inside bounds."
  (with-slots (x-min y-min x-max y-max) bounds
    (and (< (vx point) x-max)
         (< (vy point) y-max)
         (>= (vx point) x-min )
         (>= (vy point) y-min))))

(defun bounds-to-points (bounds)
  "Return a list of four boundary points."
  (with-slots (x-min y-min x-max y-max) bounds
    (list (vec2 x-min y-min)
          (vec2 x-min y-max)
          (vec2 x-max y-min)
          (vec2 x-max y-max))))

(defun random-point-in (bounds)
  "Return a list of four boundary points."
  (with-slots (x-min y-min x-max y-max) bounds
    (vec2 (+ x-min (* (- x-max x-min) (random 1.0)))
          (+ y-min (* (- y-max y-min) (random 1.0))))))

(defun random-points-in (bounds count)
  "Return a list of four boundary points."
  (with-slots (x-min y-min x-max y-max) bounds
    (loop for i below count collecting
         (vec2 (+ x-min (* (- x-max x-min) (random 1.0)))
               (+ y-min (* (- y-max y-min) (random 1.0)))))))

(defun split-bounds (bounds)
  (with-slots (x-min x-max y-min y-max) bounds
    (let ((x-mid (/ (+ x-max x-min) 2))
          (y-mid (/ (+ y-max y-min) 2)))
      (make-array 4
                  :initial-contents
                  (list 
                   (make-instance 'quadtree-bounds
                                  :x-min x-min :x-max x-mid
                                  :y-min y-mid :y-max y-max)
                   (make-instance 'quadtree-bounds
                                  :x-min x-mid :x-max x-max
                                  :y-min y-mid :y-max y-max)
                   (make-instance 'quadtree-bounds
                                  :x-min x-min :x-max x-mid
                                  :y-min y-min :y-max y-mid)
                   (make-instance 'quadtree-bounds
                                  :x-min x-mid :x-max x-max
                                  :y-min y-min :y-max y-mid))))))


(defun midpoint (bounds)
  (with-slots (x-min y-min x-max y-max) bounds
    (vec2 (/ (+ x-max x-min) 2)
          (/ (+ y-max y-min) 2))))
