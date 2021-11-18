#lang racket/base
(require racket/cmdline
         racket/match
         json
         net/base64)

(require racket/pretty)

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
   (current-output-port)))

#;
(match (current-command-line-arguments)
  [(vector net-id)
   (let ([raw-path (string-append net-id "-raw")]
         [ext-path (string-append net-id "-extracted")])
     (for-each
      (λ (id)
        (begin ;with-handlers ([exn:fail? void])
          (let ([old-path (build-path raw-path id)]
                [new-path (build-path ext-path id)])
            (unless (file-exists? new-path)
              (let* ([json (call-with-input-file old-path read-json)]
                     [json (hash-ref json 'params)]
                     [json (hash-ref json 'approval-program)])
                (call-with-output-file new-path
                  (λ (op) (write-bytes (base64-decode (string->bytes/utf-8 json)) op))))))))
      (directory-list raw-path)))])



