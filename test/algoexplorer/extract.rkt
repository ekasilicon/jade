#lang racket/base
(require racket/match
         json
         net/base64)

(define (extract json selector)
  (match (hash-ref (hash-ref json 'params) selector)
    ['null
     #f]
    [program
     (base64-decode (string->bytes/utf-8 program))]))

(define (file-extract filename selector)
  (extract (call-with-input-file filename read-json) selector))

(provide extract file-extract)

(module+ main
  (require racket/cmdline)

  (void
   (write-bytes
    (extract (read-json (current-input-port))
             (command-line
              #:args (selector)
              (match selector
                ["ApprovalProgram"
                 'approval-program]
                ["ClearStateProgram"
                 'clear-state-program]
                [_
                 (error "~a not recognized; use ApprovalProgram or ClearStateProgram" selector)])))
    (current-output-port))))
