#lang racket/base
(require (only-in racket/list take)
         "static/object.rkt")

(define ((make-*/version who x0 x1 x2 x3 x4 x5 x6 extras) v) 
  (if (and (exact-nonnegative-integer? v)
           (<= 1 v 6))
    (apply mix
           extras
           (reverse (take (list x0 x1 x2 x3 x4 x5 x6) (add1 v))))
    (error who "expected version 1, 2, 3, 4, 5, or 6; received ~v" v)))

(provide make-*/version)
