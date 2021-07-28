#lang racket/base
(require racket/match
         racket/file
         "abstract.rkt")

(fold-files
 (λ (name type u)
   (when (and (eq? type 'file)
              (regexp-match? #px"tealc$" name))
     (displayln name)
     (match (execute (file->bytes name))
       [(failure msg _)
        (raise msg)]
       [_
        (void)])))
 (void)
 "../corpus"
 #f)
