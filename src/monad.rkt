#lang racket/base
(require racket/match
         "static/object.rkt")

(define monad-extras
  (inc (>>=)
       [>> (λ (m . ms) (foldl (λ (m m₀) (>>= m₀ (λ _ m))) m ms))]))

(define monad+-extras
  (inc (mplus)
       [mzero (mplus)]))

(provide monad-extras monad+-extras)

#|
(record Monad (unit >>=))

(define (derive->> m)
  (match-define (Monad >>=) m)


(provide Monad
         derive->>)

(record Monad+ Monad (mplus))

(define (derive-mzero m+)
  (match-define (Monad+ mplus) m+)
  (mplus))

(provide Monad+
         derive-mzero)
|#
