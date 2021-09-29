#lang racket/base
(require racket/match
         "monad.rkt"
         "read-byte.rkt")

; bytes? -> (cons a bytes?) | #f

; instance Monad Prefix
(define prefix-Monad
  (Monad [unit (λ (a) (λ (bs) (cons a bs)))]
         [>>= (λ (m f)
                (λ (bs)
                  (match (m bs)
                    [(cons x bs) ((f x) bs)]
                    [#f #f])))]
         [>> (λ (m₀ m₁)
               (λ (bs)
                 (match (m₀ bs)
                   [(cons _ bs) (m₁ bs)]
                   [#f #f])))]))

(define prefix-ReadByte
  (ReadByte [Monad prefix-Monad]
            [read-byte (λ (bs)
                         (if (zero? (bytes-length bs))
                           #f
                           (cons (bytes-ref bs 0) (subbytes bs 1))))]))

(define read-prefix read-varuint)

(provide prefix-Monad
         prefix-ReadByte
         read-prefix)

(module+ main
  ((read-prefix prefix-ReadByte) (bytes 127 1 2 3))
  ((read-prefix prefix-ReadByte) (bytes 128 127 1 2 3)))
