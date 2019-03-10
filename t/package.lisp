;;;; package.lisp
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

(in-package :cl-user)
(defpackage :quadtree.test
  (:use :cl
        :fiveam
        :alexandria
        :quadtree
        :3d-vectors))

(in-package :quadtree.test)

(def-suite :quadtree)
(in-suite :quadtree)

(test point-quadtree-insert-and-size
  (let ((qt (make-instance 'point-quadtree)))

    (insert qt (vec2 0.0 0.0) 42)
    (is-true (= (qsize qt) 1))

    (insert qt (vec2 0.0 0.0) 44)
    (is-true (= (qsize qt) 2))

    (insert qt (vec2 1.0 0.0) 45)
    (is-true (= (qsize qt) 3))))


(test point-quadtree-locate
  (let ((rp1 (vec2-random 0.0 1.0))
        (rp2 (vec2-random 0.0 1.0))
        (rp3 (vec2-random 0.0 1.0))
        (qt (make-instance 'point-quadtree)))

    (insert qt rp1 42)
    (let ((lr (locate qt 42 #'=)))
      (is-true (= 1 (length lr)))
      (is-true (find rp1 lr :test #'v=)))

    (insert qt rp2 142)
    (let ((lr (locate qt 142 #'=)))
      (is-true (= 1 (length lr)))
      (is-true (find rp2 lr :test #'v=)))

    (insert qt rp3 42)
    (let ((lr (locate qt 42 #'=)))
      (is-true (= 2 (length lr)))
      (is-true (find rp3 lr :test #'v=)))))

(defun build-grid-quadtree (type width height)
  (let ((qt (make-instance type)))
    (dotimes (i width)
      (dotimes (j height)
        (insert qt (vec2 i j) (* i j))))
    qt))

(test point-quadtree-range-find
  (let ((qt (make-instance 'point-quadtree)))
    (insert qt (vec2 0.0 0.0) 1)
    (insert qt (vec2 1.0 0.0) 2)
    (insert qt (vec2 2.0 0.0) 3)
    (insert qt (vec2 3.0 0.0) 4)
    (insert qt (vec2 4.0 0.0) 5)
    (insert qt (vec2 5.0 0.0) 6)
    (is-true (= 2 (length (range-find qt (vec2 2.5 0.0) 1.0)))))

  (let* ((qt (build-grid-quadtree 'point-quadtree 5 5))
         (first-results (range-find qt (vec2 2 2) 1.0))
         (second-results (range-find qt (vec2 2.5 2.5) 1.0)))
    (is-true (= 4 (length first-results)))
    (is-true (= 4 (length second-results)))))

(test point-quadtree-closest
  (is-false
   (handler-case 
       (let ((qt (make-instance 'point-quadtree)))
         (is-true (null (closest qt (vec2 0.0 0.0))))

         (insert qt (vec2-random 0.0 1.0) 1)
         (insert qt (vec2-random 0.0 1.0) 2)
         (insert qt (vec2-random 0.0 1.0) 3)

         (insert qt (vec2 5.0 0.0) 42)
         (insert qt (vec2 10.00 0.0) 47)

         (multiple-value-bind (location values) (closest qt (vec2 6.0 0.0))
           (is-true (v= location (vec2 5.0 0.0)))
           (is-true (find 42 values :test #'=)))

         (multiple-value-bind (location values) (closest qt (vec2 9.0 0.0))
           (is-true (v= location (vec2 10.0 0.0)))
           (is-true (find 47 values :test #'=)))
         t)
     (error (e)
       (format t "Caught ~a~%" e)
       nil))))

(test point-quadtree-remove-item
  (is-false
   (handler-case
       (let ((qt (make-instance 'point-quadtree)))

         (insert qt (vec2-random 0.0 1.0) 100)
         (is-true (= 1 (qsize qt)))

         (remove-item qt 10 #'=)
         (is-true (= 1 (qsize qt)))

         (remove-item qt 100 #'=)
         (is-true (= 0 (qsize qt)))
         t)
     (error (e)
       (format t "Caught ~a~%" e)
       nil))))

(test point-quadtree-remove-from
  (is-false
   (handler-case 
       (let ((qt (make-instance 'point-quadtree)))

         (insert qt (vec2 0.0 0.0) 100)
         (is-true (= 1 (qsize qt)))

         (remove-from qt (vec2 1.0 0.0))
         (is-true (= 1 (qsize qt)))
         (remove-from qt (vec2 0.0 0.0))
         (is-true (= 0 (qsize qt)))
         t)
     (error (e)
       (format t "Caught ~a~%" e)
       nil))))


(test quadrant-of
  (is-true (eq 'top-left     (quadrant-of (vec2 0.0 0.0) (vec2 -1.0 1.0))))
  (is-true (eq 'top-right    (quadrant-of (vec2 0.0 0.0) (vec2 1.0 1.0))))
  (is-true (eq 'bottom-left  (quadrant-of (vec2 0.0 0.0) (vec2 -1.0 -1.0))))
  (is-true (eq 'bottom-right (quadrant-of (vec2 0.0 0.0) (vec2 1.0 -1.0)))))

(test quadtree-bounds
  (let* ((bounds (from-point-range (vec2 0.0 0.0) 5.0))
         (points (bounds-to-points bounds)))
    (is-true (find (vec2 -5.0 -5.0) points :test #'v=))
    (is-true (find (vec2 -5.0 5.0) points :test #'v=))
    (is-true (find (vec2 5.0 5.0) points :test #'v=))
    (is-true (find (vec2 5.0 -5.0) points :test #'v=))

    (is-true  (inside-p (vec2 2.5 2.5) bounds))
    (is-true  (inside-p (vec2 2.5 -2.5) bounds))
    (is-true  (inside-p (vec2 -2.5 2.5) bounds))
    (is-true  (inside-p (vec2 -2.5 -2.5) bounds))
    (let ((sb (split-bounds bounds)))
      (is-true (inside-p (vec2 2.5 2.5) (cdr (assoc 'quadtree:top-right sb))))
      (is-true (inside-p (vec2 -2.5 2.5) (cdr (assoc 'quadtree:top-left sb))))
      (is-true (inside-p (vec2 2.5 -2.5) (cdr (assoc 'quadtree:bottom-right sb))))
      (is-true (inside-p (vec2 -2.5 -2.5) (cdr (assoc 'quadtree:bottom-left sb)))))))


(test entries
  (let ((first-entry (make-entry (vec2 0.0 0.0) 42))
        (second-entry (make-entry (vec2 2.0 1.0) 34)))
    (add-value first-entry 76)
    (is-true (contains first-entry 42 #'=))
    (is-true (contains first-entry 76 #'=))
    (is-false (contains first-entry 34 #'=))
    (remove-value first-entry 76 #'=)
    (is-false (contains first-entry 76 #'=))

    (is-false (contains second-entry 42 #'=))
    (is-false (contains second-entry 76 #'=))
    (is-true (contains second-entry 34 #'=))
    (is-true (is-point first-entry (vec2 0.0 0.0)))
    (is-false (is-point first-entry (vec2 1.0 0.0)))
    (is-true (is-point second-entry (vec2 2.0 1.0)))))


(defun test-tree (n)
  (let ((pqt (make-instance 'quadtree:pr-quadtree)))
    (dotimes (i n)
      (quadtree:insert pqt (vec2 i i) i))
    pqt))

(test pr-quadtree-insert-and-size
  (let ((qt (make-instance 'pr-quadtree)))
    (is-true (= (qsize qt) 0))
    (insert qt (vec2 0.0 0.0) 42)
    (is-true (= (qsize qt) 1))

    (insert qt (vec2 0.0 0.0) 44)
    (is-true (= (qsize qt) 2))

    (insert qt (vec2 1.0 0.0) 45)
    (is-true (= (qsize qt) 3))))


(test pr-quadtree-range-find
  (let ((qt (make-instance 'pr-quadtree)))
    (insert qt (vec2 0.0 0.0) 1)
    (insert qt (vec2 1.0 0.0) 2)
    (insert qt (vec2 2.0 0.0) 3)
    (insert qt (vec2 3.0 0.0) 4)
    (insert qt (vec2 4.0 0.0) 5)
    (insert qt (vec2 5.0 0.0) 6)
    (is-true (= 2 (length (range-find qt (vec2 2.5 0.0) 1.0)))))

  (let* ((qt (build-grid-quadtree 'pr-quadtree 5 5))
         (first-results (range-find qt (vec2 2 2) 1.0))
         (second-results (range-find qt (vec2 2.5 2.5) 1.0)))
    (is-true (= 4 (length first-results)))
    (is-true (= 4 (length second-results)))))


