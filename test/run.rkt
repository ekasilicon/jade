#lang racket/base
(require racket/match
         racket/set
         racket/port
         racket/pretty)

(require "../src/error.rkt"
         "../src/main.rkt"
         "../src/unconstrained-property-analysis.rkt"
         #;
         "../src/debug.rkt")

(match (current-command-line-arguments)
  [(vector)
   (displayln "racket run synthetic | algoexplorer <net-id> | debug <path> ...")]
  [(vector "synthetic" args ...)
   (for-each
    (λ (path)
      (displayln path)
      (parameterize ([current-command-line-arguments (list->vector (cons "--assembly" args))]
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
              (for-each
               (match-lambda
                 [#f
                  (void)]
                 [(cons _ report)
                  (displayln report)])
               reports)])])))
    (directory-list "synthetic" #:build? #t))]
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
           (let-values ([(rs cpu real gc) (parameterize ([current-UPA-report
                                                          (match-lambda
                                                            [(list (cons 2 _))
                                                             'no-states]
                                                            [(list (cons x _)
                                                                   _)
                                                             (match x
                                                               [0 'OK]
                                                               [1 'alert]
                                                               [2 'critical])])])
                                            (time-apply action (list)))])
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
                           (let* ([results (hash-update results
                                                        'success-statuses
                                                        (λ (statuses) (hash-update statuses report add1 0))
                                                        (hasheq))]
                                  [results (hash-update results
                                                        'success-times
                                                        (λ (buckets)
                                                          (hash-update
                                                           buckets
                                                           (let loop ([ms 3])
                                                             (if (<= real ms)
                                                               ms
                                                               (loop (floor (* ms 3/2)))))
                                                           add1 0))
                                                        (hasheqv))])
                             results)]))]
                  [else
                   (hash-update results 'no-UPA add1 0)])]))]))))]
  #;
  [(vector "debug" paths ...)
   (for-each
    (λ (path)
      (displayln path)
      (debug (disassemble (file-extract path 'approval-program))))
    paths)])

 





