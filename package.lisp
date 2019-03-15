;; package.lisp 

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

(defpackage :quadtree
  (:use #:cl #:j-utils #:alexandria #:3d-vectors)
  (:export #:split-size
           #:point-quadtree
           #:pr-quadtree
           #:quadtree-bounds
           #:from-point-range
           #:inside-p
           #:bounds-to-points
           #:split-bounds
           #:qsize
           #:locate
           #:closest
           #:insert
           #:depth-first
           #:remove-item
           #:remove-from
           #:quadrant-of
           #:opposite-quadrant
           #:top-left
           #:top-right
           #:bottom-left
           #:bottom-right
           #:range-find
           #:make-entry
           #:is-point
           #:remove-value
           #:add-value
           #:contains
           #:view-quadtree
           #:random-quadtree
           #:parametric-quadtree
           #:parametric-animation))
