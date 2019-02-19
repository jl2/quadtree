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

(test insert-and-size
  (let ((qt (make-instance 'point-quadtree)))

    (insert qt (vec2 0.0 0.0) 42)
    (is-true (= (qsize qt) 1))

    (insert qt (vec2 0.0 0.0) 44)
    (is-true (= (qsize qt) 2))

    (insert qt (vec2 1.0 0.0) 45)
    (is-true (= (qsize qt) 3))))


(test locate
  (let ((rp1 (vec2-random 0.0 1.0))
        (rp2 (vec2-random 0.0 1.0))
        (rp3 (vec2-random 0.0 1.0))
        (rp4 (vec2-random 0.0 1.0))
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

(test closest
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
      (is-true (find 47 values :test #'=)))))


(test remove-item
  (let ((qt (make-instance 'point-quadtree)))

    (insert qt (vec2-random 0.0 1.0) 100)
    (is-true (= 1 (qsize qt)))

    (remove-item qt 10 #'=)
    (is-true (= 1 (qsize qt)))

    (remove-item qt 100 #'=)
    (is-true (= 0 (qsize qt)))
    (is-true (null (locate qt 100 #'=)))
    (is-true (null (closest qt (vec2-random 0.0 1.0))))

    (insert qt (vec2-random 0.0 1.0) 42)
    (insert qt (vec2-random 0.0 1.0) 42)
    (is-true (= 2 (qsize qt)))
    (remove-item qt 42 #'=)
    (is-true (null (locate qt 100 #'=)))
    (is-true (null (locate qt 42 #'=)))
    (is-true (= 0 (qsize qt)))

    (insert qt (vec2-random 0.0 1.0) 41)
    (insert qt (vec2 10.0 10.0) 42)
    (is-true (= 2 (qsize qt)))
    (remove-item qt 41 #'=)
    (is-true (= 1 (qsize qt)))
    (is-true (= (vec2 10.0 10.0) (locate qt 42 #'=)))))

(test remove-from
  (let ((qt (make-instance 'point-quadtree)))

    (insert qt (vec2 0.0 0.0) 100)
    (is-true (= 1 (qsize qt)))

    (remove-from qt (vec2 1.0 0.0))
    (is-true (= 1 (qsize qt)))

    (remove-from qt (vec2 0.0 0.0))
    (is-true (= 0 (qsize qt)))
    (is-true (null (locate qt 100 #'=)))
    (is-true (null (closest qt (vec2 0.0 0.0))))

    (insert qt (vec2-random 0.0 1.0) 42)
    (insert qt (vec2-random 0.0 1.0) 77)
    (is-true (= 2 (qsize qt)))
    (remove-item qt 77 #'=)
    (is-true (null (locate qt 77 #'=)))
    (is-true (= 42 (locate qt 42 #'=)))
    (is-true (= 1 (qsize qt)))
    (remove-item qt 42 #'=)
    (is-true (= 0 (qsize qt)))

    (insert qt (vec2-random 0.0 1.0) 41)
    (insert qt (vec2-random 0.0 1.0) 42)
    (is-true (= 2 (qsize qt)))
    (remove-item qt 41 #'=)
    (is-true (= 1 (qsize qt)))
    (is-true (= 42 (locate qt 42 #'=)))))

(test get-quadrant
  (is-true (eq 'top-left     (get-quadrant (vec2 0.0 0.0) (vec2 -1.0 1.0))))
  (is-true (eq 'top-right    (get-quadrant (vec2 0.0 0.0) (vec2 1.0 1.0))))
  (is-true (eq 'bottom-left  (get-quadrant (vec2 0.0 0.0) (vec2 -1.0 -1.0))))
  (is-true (eq 'bottom-right (get-quadrant (vec2 0.0 0.0) (vec2 1.0 -1.0)))))
