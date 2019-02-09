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
        :quadtree))

(in-package :quadtree.test)

(def-suite :quadtree)
(in-suite :quadtree)

(test insert-and-size
  (let ((qt (make-instance 'quadtree)))

    (insert qt (vec3 0.0 0.0 0.0) 42)
    (is-true (= (qsize qt) 1) 43)

    (insert qt (vec3 0.0 0.0 0.0) 44)
    (is-true (= (qsize qt) 2))

    (insert qt (vec3 0.0 1.0 0.0) 45)
    (is-true (= (qsize qt) 3))))


(test locate
  (let ((qt (make-instance 'quadtree)))
    (insert qt (vec3 0.1 10.0 2.0) 42)
    (is-true (= (locate qt 42) 42))

    (insert qt (vec3 0.2 10.0 -10.0) 142)
    (is-true (= (locate qt 142) 142))

    (insert qt (vec3 10.1 38.2 -12.0) 42)
    (is-true (= (locate qt 42) 42))

    (insert qt (vec3 10.0 10.0 -10.0) 42)
    (is-true (equal (locate qt 42) (list 42 42)))))

(test closest
  (let ((qt (make-instance 'quadtree)))
    (is-true (null (closest qt (vec3 0.0 0.0 0.0))))

    (insert qt (vec3 0.0 0.0 0.0) 42)
    (insert qt (vec3 0.0 10.00 0.0) 47)

    (multiple-value-bind (location value) (closest qt (vec3 0.0 1.0 0.0))
      (is-true (v= location (vec3 0.0 0.0 0.0)))
      (is-true (= value 42)))

    (multiple-value-bind (location value) (closest qt (vec3 0.0 9.0 0.0))
      (is-true (v= location (vec3 0.0 10.0 0.0)))
      (is-true (= value 47)))))


(test remove-item
  (let ((qt (null (closest qt (vec3 0.0 0.0 0.0)))))

    (insert qt (vec3 0.0 0.0 0.0) 100)
    (is-true (= 1 (qsize qt)))
    (remove-item qt 100)

    (is-true (= 0 (qsize qt)))
    (is-true (null (locate qt 100)))
    (is-true (null (closest qt (vec3 0.0 0.0 0.0))))

    (insert qt (vec3 70.2 0.0 -10.0) 42)
    (insert qt (vec3 -18.2 -3.5 -10.0) 42)
    (is-true (= 2 (qsize qt)))
    (remove-item qt 42)
    (is-true (null (locate qt 100)))
    (is-true (null (locate qt 42)))
    (is-true (= 0 (qsize qt)))

    (insert qt (vec3 -17.0 0.0 -10.0) 41)
    (insert qt (vec3 18.0 -3.5 -10.0) 42)
    (is-true (= 2 (qsize qt)))
    (remove-item qt 41)
    (is-true (= 1 (qsize qt)))
    (is-true (= 42 (locate qt 42)))))
