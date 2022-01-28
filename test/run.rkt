#lang racket/base
(require racket/match
         racket/set
         racket/port
         racket/pretty)

(require "algoexplorer/extract.rkt"
         "../src/error.rkt"
         "../src/parse.rkt"
         "../src/disassemble.rkt"
         "../src/unconstrained-property-analysis.rkt"
         "../src/debug.rkt")

(match (current-command-line-arguments)
  [(vector)
   (displayln "racket run showcase | algoexplorer <net-id> | debug <path> ...")]
  [(vector "showcase")
   (for-each
    (λ (path)
      (displayln path)
      (let ([rs (time (analyze/assembly (call-with-input-file path port->string)))])
        (for ([r (in-set rs)])
          (pretty-print r))))
    (directory-list "showcase" #:build? #t))]
  [(vector "algoexplorer" net)
   (pretty-print
    (for/fold ([results (hasheq)])
              ([path (in-list (directory-list (build-path "algoexplorer" net) #:build? #t))])
      (displayln path)
      (let-values ([(rss cpu real gc) (time-apply analyze/json-package (list (call-with-input-file path port->bytes) (hash)))])
        (match rss
          [(list (error-result tag message))
           (displayln tag)
           (displayln message)
           (hash-update results tag add1 0)]
          [(list rs)
           (if (zero? (set-count rs))
             (begin
               (displayln 'no-states)
               (displayln "no states")
               (hash-update results 'no-states add1 0))
             (hash-update results
                          'successes
                          (λ (buckets)
                            (hash-update
                             buckets
                             (let loop ([ms 3])
                               (if (<= real ms)
                                 ms
                                 (loop (floor (* ms 3/2)))))
                             add1 0))
                            (hasheqv))
             #;
             (begin
               
               (pretty-print (disassemble (file-extract path 'approval-program))))
             #;
               (begin
                 (displayln n)
                 (for ([r (in-set rs)])
                   (pretty-print r))))]))
      ))]
  [(vector "debug" paths ...)
   (for-each
    (λ (path)
      (displayln path)
      (debug (disassemble (file-extract path 'approval-program))))
    paths)])

 





