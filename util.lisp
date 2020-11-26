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

(defun random-quadtree (type radius count)
  "Create a random quadtree of specified type', with 'count' points within
-radius and radius of the origin along both axes."
  (let ((qt (make-instance type)))
    (dotimes (i count)
      (quadtree:insert qt (vec2-random (- radius) radius) i))
    qt))

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
  "Create a quadtree of specified type, where all of the points lie on the curve
specified by parametric function (xf(t), yf(t))"
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
  "Generate frames of an animation showing quadtree refinement as points are added.
Animation shows points on a parametric curve, generated using parametric-quadtree."
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

(defun benchmark-quadtree-search ()
  (dotimes (i 16)
    (let* ((quadtree::*split-size* (* (1+ i) 2))
           (qt (quadtree:parametric-quadtree)))
      (dotimes (i 10)
        (let ((pt (vec2-random -10.0 10.0))
              (rg (+ 0.1 (random 10.0))))
          (let ((results (quadtree:range-find qt pt rg )))
            ))))))
一
