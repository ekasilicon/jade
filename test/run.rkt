#lang racket/base
(require racket/match
         racket/set
         racket/port
         racket/pretty)

(require "../src/error.rkt"
         "../src/main.rkt"
         #;
         "../src/debug.rkt")

(match (current-command-line-arguments)
  [(vector)
   (displayln "racket run showcase | algoexplorer <net-id> | debug <path> ...")]
  [(vector "showcase")
   (for-each
    (λ (path)
      (displayln path)
      (parameterize ([current-command-line-arguments (vector "--assembly")]
                     [current-input-port (open-input-bytes (call-with-input-file path port->bytes))])
        (match (command-line)
          [(error-result tag message)
           (display "ERROR ")
           (displayln message)]
          [action
           (match (time (action))
             [(error-result tag message)
              (display "ERROR ")
              (displayln message)]
             [reports
              (for-each displayln (map cdr reports))])])))
    (directory-list "showcase" #:build? #t))]
  [(vector "algoexplorer" net)
   (pretty-print
    (for/fold ([results (hasheq)])
              ([path (in-list (directory-list (build-path "algoexplorer" net) #:build? #t))])
      (displayln path)
      (parameterize ([current-command-line-arguments (vector "--json-package")]
                     [current-input-port (open-input-bytes (call-with-input-file path port->bytes))])
        (match (command-line)
          [(error-result tag message)
           (displayln tag)
           (displayln message)
           (hash-update results tag add1 0)]
          [action
           (let-values ([(rs cpu real gc) (time-apply action (list))])
             (match rs
               [(list (error-result tag message))
                (displayln tag)
                (displayln message)
                (hash-update results tag add1 0)]
               [(list reports)
                (cond
                  [(assq 'UPA reports)
                   => (λ (X×UPA)
                        (match (cdr X×UPA)
                          [(error-result tag message)
                           (displayln tag)
                           (displayln message)
                           (hash-update results tag add1 0)]
                          [report
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
                                        (hasheqv))]))]
                  [else
                   (hash-update results 'no-UPA add1 0)])]))]))))]
  #;
  [(vector "debug" paths ...)
   (for-each
    (λ (path)
      (displayln path)
      (debug (disassemble (file-extract path 'approval-program))))
    paths)])

 





