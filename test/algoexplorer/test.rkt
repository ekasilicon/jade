#lang racket/base
(require racket/cmdline
         racket/pretty
         "extract.rkt"
         "../../src/disassemble.rkt")

(command-line
 #:program "jade test"
 #:args (net-id)
 (let ([count (for/fold ([count 0])
                        ([path (in-list (directory-list net-id #:build? #t))])
                (cond
                  [(file-extract path 'approval-program)
                   => (λ (bs)
                        (time (disassemble-bytes bs))
                        (add1 count))]
                  [else
                   count]))])
   (printf "disassembled ~a program~a\n"
           count
           (if (= count 1) "" "s"))))


