#lang racket/base
(require racket/match)

(define (global key)
  (unit (list 'global key)))

(match bc
  [#x32 ; global
   (>>= (>>= read-uint8
             (λ (i) (>>= (global-index→key i) global)))
        push)])
