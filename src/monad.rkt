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
