# quadtree
### _Jeremiah LaRocco <jeremiah_larocco@fastmail.com>_

Simple QuadTree library.

```commonlisp
* (ql:quickload quadtree)
* (quadtree:parametric-animation "~/images/quadtrees/animation/"
                                 :frames 400 :width 2400 :height 2400
                                 :x-scale 30.0 :y-scale 30.0
                                 :xf (lambda (tv) (* 22.0 (sin tv) (sin (* 3 tv))))
                                 :yf (lambda (tv) (* 22.0 (cos tv) (sin (* 3 tv))))))
NIL
*
```
## License

ISC


Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


