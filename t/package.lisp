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

(test insert
  (let ((qt (make-instance 'quadtree)))

    (insert qt (vec3 0.0 0.0 0.0))

    (is-true (= (qsize qt) 1))

    (insert qt (vec3 0.0 0.0 0.0))
    (is-true (= (qsize qt) 2))

    (insert qt (vec3 0.0 1.0 0.0))
    (is-true (= (qsize qt) 3))))

