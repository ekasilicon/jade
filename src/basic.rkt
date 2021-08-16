#lang racket/base
(require racket/match
         "monad.rkt"
         "read-byte.rkt"
         "vm.rkt")

; this module uses the coarsest value abstraction
; possible and consequently exhibits the lower bound
; of precision.

; Result
(struct success (x) #:transparent)
(struct failure (msg) #:transparent)
(struct partial (xs s) #:transparent)


(define (snull on-cons on-null) (on-null))
(define ((scons x xs) on-cons on-null) (on-cons x xs))
(define (sunit x) (scons x snull))
(define (sappend xs ys)
  (xs (λ (x xs) (scons x (sappend xs ys)))
      (λ () ys)))

; instance Monad VM ...
(define monad
  (Monad
   ; unit
   (λ xs (λ (p? s) (sunit (partial xs s))))
   ; >>=
   (λ (m f)
     (λ (p? s)
       (m p? s
          (match-lambda
            [(partial xs s)
             ((apply f xs) p? s)]
            [r (sunit r)]))))
   ; >>
   (λ (m₀ m₁)
     (λ (p? s)
       (m₀ p? s
           (match-lambda
             [(partial xs s)
              (m₁ p? s)]
             [r (sunit r)]))))))

; instance Monad+ VM
(define monad+
  (MonadPlus
   monad
   ; fail
   (λ (p? s)
     snull)
   ; each
   (λ (m₀ m₁)
     (λ (p? s)
       (sappend (m₀ p? s)
                (m₁ p? s))))))

; instance VM
(define basic
  (VM monad+
      (ReadByte monad )
   (ReadByte
    (Monad))))

(define (analyze bs)
  42)
