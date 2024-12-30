;; visualization.lisp

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

(defun draw-bound (bnd)
  (with-slots (x-min y-min x-max y-max) bnd
    (cl-cairo2:set-source-rgba 0 1 0 1.0)
    (cl-cairo2:move-to x-min y-min)
    (cl-cairo2:line-to x-max y-min)
    (cl-cairo2:line-to x-max y-max)
    (cl-cairo2:line-to x-min y-max)
    (cl-cairo2:line-to x-min y-min)
    (cl-cairo2:stroke)))

(defun draw-point (pt size)
  (cl-cairo2:set-source-rgba 1 0 0 1.0)
  (cl-cairo2:move-to (- (vx pt) size)
                     (- (vy pt) size))
  (cl-cairo2:line-to (+ (vx pt) size)
                     (+ (vy pt) size))
  (cl-cairo2:move-to (+ (vx pt) size)
                     (- (vy pt) size))
  (cl-cairo2:line-to (- (vx pt) size)
                     (+ (vy pt) size))
  (cl-cairo2:stroke))

(defun draw-cross (pt bound)
  (with-slots (x-min y-min x-max y-max) bound
    (cl-cairo2:set-source-rgba 0 0 1 1.0)
    (cl-cairo2:move-to x-min (vy pt))
    (cl-cairo2:line-to x-max (vy pt))
    (cl-cairo2:move-to (vx pt) y-min)
    (cl-cairo2:line-to (vx pt) y-max)
    (cl-cairo2:stroke)))

;;                |
;;                |
;;                |
;;                |
;;                |
;;                |
;; ---------------+---------------
;;        |       |
;;        |       |
;;        |       |
;; -------+-------|
;;        |   |   |
;;        |---+---|
;;        |   |   |

(defmethod view-quadtree ((qt point-quadtree)
                          png-file-name
                          &key
                            (width 1200) (height 1200)
                            (open-png t)
                            (x-scale 1.0)
                            (y-scale 1.0)
                            (line-width 1.5))
  (let ((real-file-name (home-dir png-file-name))
        (half-width (/ width 2.0))
        (half-height (/ height 2.0)))
    (ensure-directories-exist real-file-name)
    (cl-cairo2:with-png-file (real-file-name :argb32 width height)
      (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
      (cl-cairo2:paint)
      (cl-cairo2:set-line-width (/ line-width x-scale))
      (cl-cairo2:translate half-width half-height)
      (cl-cairo2:scale x-scale y-scale)
      (labels
          ((draw-quadtree (qt bound)
             (with-slots (entry children) qt
               (with-slots (point) entry
                 (draw-point point (/ 4 x-scale))

                 (let ((top-left (aref children *top-left*))
                       (bottom-left (aref children *bottom-left*))
                       (top-right (aref children *top-right*))
                       (bottom-right (aref children *bottom-right*)))
                   (when (or top-left bottom-left top-right bottom-right)
                     (draw-cross point bound)

                     (with-slots (x-min y-min x-max y-max) bound
                       (when top-left
                         (draw-quadtree top-left (make-instance 'quadtree-bounds
                                                                :x-min x-min
                                                                :x-max (vx point)
                                                                :y-min (vy point)
                                                                :y-max y-max)))
                       (when top-right
                         (draw-quadtree top-right (make-instance 'quadtree-bounds
                                                                 :x-min (vx point)
                                                                 :x-max x-max
                                                                 :y-min (vy point)
                                                                 :y-max y-max)))
                       (when bottom-left
                         (draw-quadtree bottom-left (make-instance 'quadtree-bounds
                                                                   :x-min x-min
                                                                   :x-max (vx point)
                                                                   :y-min y-min
                                                                   :y-max (vy point))))
                       (when bottom-right
                         (draw-quadtree bottom-right (make-instance 'quadtree-bounds
                                                                    :x-min (vx point)
                                                                    :x-max x-max
                                                                    :y-min y-min
                                                                    :y-max (vy point)))))))))))
        (draw-quadtree qt (make-instance 'quadtree-bounds
                                         :x-min -1000.0
                                         :x-max 1000.0
                                         :y-min -1000.0
                                         :y-max 1000.0))))
    (when open-png
      (swank:eval-in-emacs `(find-file-other-window ,(namestring real-file-name) t)))))

(defmethod view-quadtree ((qt pr-quadtree)
                          png-file-name
                          &key
                            (width 1200) (height 1200)
                            (open-png t)
                            (x-scale 1.0)
                            (y-scale 1.0)
                            (line-width 1.5))
  (let ((real-file-name (home-dir png-file-name))
        (half-width (/ width 2.0))
        (half-height (/ height 2.0)))
    (ensure-directories-exist real-file-name)
    (cl-cairo2:with-png-file (real-file-name :argb32 width height)
      (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
      (cl-cairo2:paint)
      (cl-cairo2:set-line-width (/ line-width x-scale))
      (cl-cairo2:translate half-width half-height)
      (cl-cairo2:scale x-scale y-scale)
      (labels
          ((draw-quadtree (qt)
             (with-slots (entries children bounds) qt
               (draw-bound bounds)

               (dolist (entry entries)
                 (draw-point (slot-value entry 'point) (/ 4 x-scale)))

               (loop
                  for sub-tree across children
                  when (and sub-tree
                            (not (zerop (qsize sub-tree))))
                  do
                    (draw-quadtree sub-tree)))))
        (draw-quadtree qt)))
    (when open-png
      (swank:eval-in-emacs `(find-file-other-window ,(namestring real-file-name) t)))))
