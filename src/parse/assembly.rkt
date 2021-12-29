#lang racket/base
(require racket/match
         "../static/record.rkt"
         "base.rkt"
         "uint.rkt"
         "bytes.rkt"
         "instruction.rkt")

(record pragma (content))

(define pragma-directive
  (>> (literal "#pragma ")
      (lift (λ (cs) (pragma [content (apply string cs)])) (p* (p- (cc void) line-sentinel)))))

(module+ test
  (parse-success pragma-directive
                 "#pragma hello"
                 (pragma [content "hello"]))

  (parse-failure pragma-directive
                 "#pragmahello"
                 #rx""))

(define comment
  (>> (literal "//")
      (lift (λ (cs) (apply string cs)) (p* (p- (cc void) line-sentinel)))))

(define label-declaration
  (>>0 label-identifier
       space*
       (literal ":")))

(record varuint-immediate (value))

(define int-parser
  (make-instruction-parser "int" (λ (value) (varuint-immediate value)) guarded-varuint))

(module+ test
  (parse-success int-parser
                 "int 25"
                 (varuint-immediate [value 25])))

(record bytes-immediate (value))

(define byte-parser
  (make-instruction-parser "byte" (λ (value) (bytes-immediate value)) guarded-bytes))

(module+ test
  (parse-success byte-parser
                 "byte base64 ZWE="
                 (bytes-immediate [value #"ea\0"]))

  (parse-success (>> whitespace*
                     (>>0 (p? byte-parser)
                          whitespace*
                          (p? comment)
                          line-sentinel))
                 "\tbyte base64 ZWE=\n"
                 (bytes-immediate [value #"ea\0"])))

(require (only-in racket/list take)
         "../static/object.rkt"
         "../instruction-control.rkt")

(define (resolve-control-flow lsv directives)
  (define-values (offset-map has-inorder-successor?)
    (let ([o (fix (apply mix (reverse (take instruction-control-mixins lsv))))])
      (values (o 'offset-map)
              (o 'has-inorder-successor?))))
  (let ([initial-ph (make-placeholder #f)])
    (let ([phs (let loop ([directives directives]
                          [ph initial-ph]
                          [phs (hasheq)])
                 (match directives
                   [(list)
                    (placeholder-set! ph (list))
                    phs]
                   [(cons directive directives)
                    (let* ([next-ph (make-placeholder #f)]
                           [phs (match directive
                                  [(label ℓ)
                                   (placeholder-set! ph next-ph)
                                   (hash-set phs ℓ next-ph)]
                                  [(pragma)
                                   (placeholder-set! ph next-ph)
                                   phs]
                                  [instruction
                                   (placeholder-set! ph (cons instruction next-ph))
                                   phs])])
                      (loop directives next-ph phs))]))])
      (let loop ([ph initial-ph])
        (match (placeholder-get ph)
          [(? placeholder? ph)
           (loop ph)]
          [(list)
           (void)]
          [(cons instr next-ph)
           (let ([instr (offset-map
                         (match-lambda
                           [(label ℓ)
                            (cond
                              [(hash-ref phs ℓ #f)
                               => values]
                              [else
                               (error 'parse "unknown label ~a" ℓ)])])
                         instr)])
             (placeholder-set!
              ph
              (if (has-inorder-successor? instr)
                (cons instr next-ph)
                (list))))
           (loop next-ph)]))
      (make-reader-graph initial-ph))))

(define (parse input)
  ((>> whitespace* (>>0 pragma-directive line-sentinel))
   input 0
   (λ (xs i fk)
     (match xs
       [(list (pragma content))
        (match (regexp-match #px"^version (\\d+)$" content)
          [(list _ (app string->number lsv))
           (let ([directive (∨ (∨ (instruction-parser/version lsv)
                                  int-parser
                                  byte-parser)
                               pragma-directive
                               label-declaration)])
             (let ([line (>> whitespace*
                             (>>0 (p? directive)
                                  whitespace*
                                  (p? comment)
                                  line-sentinel))])
               (resolve-control-flow
                lsv
                (let loop ([i i])
                  ((>> whitespace* end-of-input)
                   input i
                   (λ (_ i fk) (list))
                   (λ ()
                     (line input i (λ (ds i fk)
                                     (match ds
                                       [(list directive)
                                        (if directive
                                          (cons directive (loop i))
                                          (loop i))]
                                       [_
                                        (error 'parser "expected single result")]))
                           (λ ()
                             (error (report input i
                                            "an instruction"
                                            "a comment signalled by //"
                                            "a #pragma directive")))))))) ))]
          [#f
           (error (report input 0 "#pragma version <teal-version>"))])]))
   (λ () (error (report input 0 "#pragma version <teal-version>")))))

(provide parse)

(module+ main
  (require racket/port
           racket/pretty)

  (let ([input (port->string (current-input-port))])
    (let ([directives (time (parse input))])
      (pretty-print directives)
      #;
      (pretty-print (resolve-control-flow directives)))))
