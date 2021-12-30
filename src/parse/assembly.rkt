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

(require "../instruction-control.rkt")

(define (resolve-control-flow lsv directives)
  (let* ([initial-ph (make-placeholder #f)]
         [phs (let loop ([directives directives]
                         [ph initial-ph]
                         [phs (hash)])
                (match directives
                  [(list)
                   (placeholder-set! ph (list))
                   phs]
                  [(cons directive directives)
                   (match directive
                     [(label ℓ)
                      (loop directives ph (hash-set phs (label ℓ) ph))]
                     [(pragma)
                      (loop directives ph phs)]
                     [instruction
                      (let ([next-ph (make-placeholder #f)])
                        (placeholder-set! ph (cons instruction next-ph))
                        (loop directives next-ph phs))])]))])
    (resolve-CFG-placeholders lsv phs initial-ph)))

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
