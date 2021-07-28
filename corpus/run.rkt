#lang racket/base
(require racket/match
         racket/file
         racket/runtime-path)

(define-runtime-path here-path ".")

(define (run f u)
  (define (fold-corpus corpus-name)
    (fold-files
     (λ (name type u)
       (if (and (eq? type 'file)
                (regexp-match? #px"tealc$" name))
         (f u name (file->bytes name))
         u))
     u
     (build-path here-path corpus-name)
     #f))
  (let ([u (fold-corpus "reach-examples")])
    u))

(provide run)

