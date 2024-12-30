;; util.lisp

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

(defun parametric-quadtree (&key
                              (type 'pr-quadtree)
                              (t-min 0.0)
                              (t-max (* 2 pi))
                              (steps 1000)
                              (xf (lambda (tv) (* 20.0 (sin tv) (sin (* 2 tv)))))
                              (yf (lambda (tv) (* 20.0 (cos tv) (sin (* 2 tv)))))
                              (bounds (make-instance 'quadtree:quadtree-bounds
                                                     :x-min -25.0
                                                     :x-max 25.0
                                                     :y-min -25.0
                                                     :y-max 25.0)))
  (let* ((qt (if (eq type 'pr-quadtree)
                 (make-instance 'quadtree:pr-quadtree :bounds bounds)
                 (make-instance type)))
         (dt (/ (- t-max t-min) steps)))
    (loop for i below steps
       for tv = (+ t-min (* dt i))
       do 
         (quadtree:insert qt (vec2 (funcall xf tv) (funcall yf tv)) i))
    qt))

(defun parametric-animation (output-directory
                             &key
                               (type 'pr-quadtree)
                               (t-min 0.0)
                               (t-max (* 2 pi))
                               (width 1200)
                               (height 1200)
                               (xf (lambda (tv) (* 20.0 (sin tv) (sin (* 2 tv)))))
                               (yf (lambda (tv) (* 20.0 (cos tv) (sin (* 2 tv)))))
                               (bounds (make-instance 'quadtree:quadtree-bounds
                                                      :x-min -25.0
                                                      :x-max 25.0
                                                      :y-min -25.0
                                                      :y-max 25.0))
                               (x-scale 20.0)
                               (y-scale 20.0)
                               (frames 60))
  (dotimes (i frames)
    (let ((qt (quadtree:parametric-quadtree :t-min t-min
                                            :t-max t-max
                                            :type type
                                            :steps (+ 4 (* i 4))
                                            :xf xf
                                            :yf yf
                                            :bounds bounds))
          (file-name (format nil "~aframe~5,'0d.png" output-directory i)))
      (quadtree:view-quadtree qt file-name :x-scale x-scale :y-scale y-scale :width width :height height :open-png nil))))

(defun insert-random-points (qt count bounds)
  (loop
     for pt in (random-points-in bounds count)
     do
       (insert qt pt 0)))

(defun random-quadtree (type radius count)
  (let* ((bounds (from-point-range (vec2 0.0 0.0) radius))
         (qt (if (eq type 'point-quadtree)
                 (make-instance 'point-quadtree)
                 (make-instance 'pr-quadtree :bounds bounds))))
    (insert-random-points qt count bounds )
    qt))



(defun benchmark-random-pr-quadtree-search ( point-count find-count)
  (dotimes (i 16)
    (let* ((quadtree::*split-size* (* (1+ i) 2))
           (radius 100.0))
      (format t "Split size: ~a~%Building quadtree of size ~a" quadtree::*split-size* point-count)
      (let ((qt (time (random-quadtree 'pr-quadtree radius point-count))))
        (format t "Performing ~a random range-finds~%" find-count)
        (time
         (dotimes (i find-count)
           (let ((pt (vec2-random (- radius) radius))
                 (rg (+ 0.1 (random radius))))
             (quadtree:range-find qt pt rg ))))
        qt))))

(defun benchmark-random-point-quadtree-search ( point-count find-count)
  (let* ((radius 100.0))
    (format t "Building quadtree of size ~a" point-count)
    (let ((qt (time (random-quadtree 'point-quadtree radius point-count))))
      (format t "Performing ~a random range-finds~%" find-count)
      (time
       (dotimes (i find-count)
         (let ((pt (vec2-random (- radius) radius))
               (rg (+ 0.1 (random radius))))
           (quadtree:range-find qt pt rg ))))
      qt)))

(defun build-grid-quadtree (type bounds x-count y-count)
  (with-slots (x-min x-max y-min y-max) bounds
    (let ((qt (if (equal type 'point-quadtree)
                (make-instance 'point-quadtree)
                (make-instance 'pr-quadtree :bounds bounds)))
          (dx (/ (- x-max x-min) x-count))
          (dy (/ (- y-max x-min) y-count)))
      (dotimes (i x-count)
        (let ((x-value (+ x-min (* dx i))))
          (dotimes (j y-count)
            (let ((y-value (+ y-min (* dy j))))
              (insert qt (vec2 x-value y-value) 0)))))
      qt)))

(defun benchmark-grid-quadtree-search (x-count y-count find-count)
  (let* ((radius 100.0)
         (bounds (from-point-range (vec2 0.0 0.0) radius))
         (pt-qt (build-grid-quadtree 'point-quadtree bounds x-count y-count))
         (pr-qt (build-grid-quadtree 'pr-quadtree bounds x-count y-count)))
    (dotimes (i find-count)
      (let* ((pt (vec2-random (- radius) radius))
             (rg (+ 0.1 (random radius)))
             (pt-find (quadtree:range-find pt-qt pt rg ))
             (pr-find (quadtree:range-find pr-qt pt rg )))
        (assert (= (length pt-find) (length pr-find)))))
    (values pt-qt pr-qt)))

