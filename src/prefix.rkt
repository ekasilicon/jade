#lang racket/base
(require racket/match
         "static/object.rkt"
         "monad.rkt"
         "read-byte.rkt")

(define prefix-read-byte
  (mix read-byte-extras
       (inc ()
            [read-byte
             (λ (bs)
               (if (zero? (bytes-length bs))
                 #f
                 (cons (bytes-ref bs 0) (subbytes bs 1))))])
       monad-extras
       (inc ()
            [unit
             (λ (a) (λ (bs) (cons a bs)))]
            [>>=
             (λ (m f)
               (λ (bs)
                 (match (m bs)
                   [(cons x bs) ((f x) bs)]
                   [#f #f])))])))

(provide prefix-read-byte)

(module+ main
  (((fix prefix-read-byte) 'read-varuint) (bytes 127 1 2 3))
  (((fix prefix-read-byte) 'read-varuint) (bytes 128 127 1 2 3)))
