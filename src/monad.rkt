#lang racket/base
(require racket/match
         "static/record.rkt")

(record Monad (unit >>=))

(define (>> m)
  (match-define (Monad >>=) m)
  (λ (m . ms) (foldl (λ (m m₀) (>>= m₀ (λ _ m))) m ms)))

(provide Monad
         >>)

(record Monad+ (monad mplus))

(define (mzero m+)
  (match-define (Monad+ mplus) m+)
  (mplus))

(provide Monad+
         mzero)
