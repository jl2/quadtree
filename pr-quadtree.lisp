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
    (>= (length entries) *split-size*)))

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
      ((and (not (null entries)) (< (length entries) *split-size*))
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




(defun home-dir (path)
  "Utility function to make relative path names relative to the user's home directory to work around Cairo weirdness."
  (merge-pathnames path (user-homedir-pathname)))

(defun view-quadtree (qt
                      png-file-name
                      &key
                        (width 1200) (height 1200)
                        (open-png t)
                        (x-scale 1.0)
                        (y-scale 1.0)
                        (line-width 1.5))
  "Test writing a PNG file with Cairo."
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
               (with-slots (entries bounds) qt
                 (cl-cairo2:set-source-rgba 0 1 0 1.0)
                 (draw-bound bounds)

                 (dolist (entry entries)
                   (cl-cairo2:set-source-rgba 1 0 0 1.0)
                   (draw-point (slot-value entry 'point) (/ 2 x-scale)))

                 (loop for quad in '(top-left bottom-left top-right bottom-right)
                    for sub-tree = (slot-value qt quad) then (slot-value qt quad)
                    when (and sub-tree (not (zerop (qsize sub-tree))))
                    do
                      (draw-quadtree sub-tree)))))
          (draw-quadtree qt)))
      (when open-png
        (swank:eval-in-emacs (list 'find-file-other-window (namestring real-file-name))))))

(defun draw-bound (bnd)
  (with-slots (x-min y-min x-max y-max) bnd
    (cl-cairo2:move-to x-min y-min)
    (cl-cairo2:line-to x-max y-min)
    (cl-cairo2:line-to x-max y-max)
    (cl-cairo2:line-to x-min y-max)
    (cl-cairo2:line-to x-min y-min)
    (cl-cairo2:stroke)))

(defun draw-point (pt size)
  
  (cl-cairo2:move-to (- (vx pt) size)
                     (- (vy pt) size))
  (cl-cairo2:line-to (+ (vx pt) size)
                     (+ (vy pt) size))
  (cl-cairo2:move-to (+ (vx pt) size)
                     (- (vy pt) size))
  (cl-cairo2:line-to (- (vx pt) size)
                     (+ (vy pt) size))
  (cl-cairo2:stroke))

(defun parametric-quadtree (&key
                       (t-min (- pi))
                       (t-max pi)
                       (steps 1000)
                       (xf #'identity)
                       (yf #'sin)
                       (bounds (make-instance 'quadtree:quadtree-bounds
                                              :x-min (* -1.5 pi)
                                              :x-max (* 1.5 pi)
                                              :y-min -2.0
                                              :y-max 2.0)))
  (let* ((steps 1000)
         (qt (make-instance 'quadtree:pr-quadtree :bounds bounds))
         (dt (/ (- t-max t-min) steps)))
    (loop for i below 1000
       for tv = (+ t-min (* dt i))
       do 
         (quadtree:insert qt (vec2 (funcall xf tv) (funcall yf tv)) i))
    qt))
