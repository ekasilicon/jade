#lang racket/base
(require (only-in racket/list take)
         "static/object.rkt")

(define ((make-*/version who pre x1 x2 x3 x4 x5 x6 x7 post) v) 
  (if (and (exact-nonnegative-integer? v)
           (<= 1 v 7))
    (apply mix
           post
           (reverse (cons pre (take (list x1 x2 x3 x4 x5 x6 x7) v))))
    (error who "expected version 1, 2, 3, 4, 5, 6, or 7; received ~v" v)))

(provide make-*/version)
