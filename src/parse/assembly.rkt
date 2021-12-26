#lang racket/base
(require racket/match
         "../static/record.rkt"
         "base.rkt"
         "number.rkt"
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
                                           "a #pragma directive")))))))))]
          [#f
           (error (report input 0 "#pragma version <teal-version>"))])]))
   (λ () (error (report input 0 "#pragma version <teal-version>")))))

#;
(define (resolve-control-flow directives)
  (let ([initial-ph (make-placeholder #f)])
    (let ([phs (let loop ([directives directives]
                          [ph initial-ph]
                          [phs (hasheq)])
                 (match directives
                   [(list)
                    (placeholder-set! ph (list))
                    phs]
                   [(cons direc directives)
                    (let* ([next-ph (make-placeholder #f)]
                           [phs (sumtype-case Directive direc
                                  [(label ℓ)
                                   (placeholder-set! ph next-ph)
                                   (hash-set phs ℓ next-ph)]
                                  [else
                                   (placeholder-set! ph (cons direc next-ph))
                                   phs])])
                      (loop directives next-ph phs))]))])
      (let loop ([ph initial-ph])
        (match (placeholder-get ph)
          [(? placeholder? ph)
           (loop ph)]
          [(list)
           (void)]
          [(cons instr next-ph)
           (sumtype-case Directive instr
             [(i:bnz [offset ℓ])
              (cond
                [(hash-ref phs ℓ #f)
                 => (λ (is-ph)
                      (placeholder-set! ph (cons (i:bnz [offset is-ph]) next-ph)))]
                [else
                 (error 'parse "unknown label ~a" ℓ)])]
             [(i:bz [offset ℓ])
              (cond
                [(hash-ref phs ℓ #f)
                 => (λ (is-ph)
                      (placeholder-set! ph (cons (i:bz [offset is-ph]) next-ph)))]
                [else
                 (error 'parse "unknown label ~a" ℓ)])]
             [(i:callsub [offset ℓ])
              (cond
                [(hash-ref phs ℓ #f)
                 => (λ (is-ph)
                      (placeholder-set! ph (cons (i:callsub [offset is-ph]) next-ph)))]
                [else
                 (error 'parse "unknown label ~a" ℓ)])]
             [(i:b [offset ℓ])
              (cond
                [(hash-ref phs ℓ #f)
                 => (λ (is-ph)
                      (placeholder-set! ph is-ph))]
                [else
                 (error 'parse "unknown label ~a" ℓ)])]
             [(i:err)
              (placeholder-set! ph (cons (i:err) (list)))]
             [(i:return)
              (placeholder-set! ph (cons (i:return) (list)))]
             [(i:retsub)
              (placeholder-set! ph (cons (i:retsub) (list)))]
             [else
              (void)])
           (loop next-ph)]))
      (make-reader-graph initial-ph))))

(module+ main
  (require racket/port
           racket/pretty)

  (let ([input (port->string (current-input-port))])
    (let ([directives (time (parse input))])
      (pretty-print directives)
      #;
      (pretty-print (resolve-control-flow directives)))))

#;
(module+ main
  (require racket/port
           racket/pretty)

  (define-syntax-rule (test-succeed expr)
    (with-handlers ([exn:fail? (λ (e)
                                 (displayln "TEST FAILED")
                                 (displayln (exn-message e)))])
      (void expr)))

  (define-syntax-rule (test-fail expr)
    (unless (with-handlers ([exn:fail? (λ (_) #t)])
              expr
              #f)
      (displayln "TEST WAS EXPECTED TO FAIL BUT DID NOT")))

  (test-succeed (parse-varuint "123456"))
  (test-succeed (parse-varuint "0123456"))
  (test-succeed (parse-varuint "0x123456"))
  (test-fail    (parse-varuint "0x1234G"))
  (test-fail    (parse-varuint "1234 1234"))
  (test-fail    (parse-varuint "xyzbc"))

  (test-succeed (parse-bytes "base64 ABCD"))
  (test-succeed (parse-bytes "b64 ABCD"))
  (test-succeed (parse-bytes "base64(ABCD)"))
  (test-succeed (parse-bytes "b64(ABCD)"))
  (test-succeed (parse-bytes "base32 ABCD"))
  (test-succeed (parse-bytes "b32 ABCD"))
  (test-succeed (parse-bytes "base32(ABCD)"))
  (test-succeed (parse-bytes "b32(ABCD)"))
  (test-succeed (parse-bytes "0xABCDEF12345678"))
  (test-fail    (parse-bytes "0xABCDEF123456780"))
  (test-fail    (parse-bytes "bose64(ABCD)"))
  (test-succeed (parse-bytes "\"\""))

  (parse-bytes "\"\"")
  (parse-bytes "\"abc\"")
  (parse-bytes "\"abc\\n\"")
  (parse-bytes "\"abc\\n\\r\"")
  (parse-bytes "\"abc\\n\\r\\t\"")
  (parse-bytes "\"abc\\n\\r\\t\\037x7\"")
  (parse-bytes "\"abc\\n\\r\\t\\x10\"")
  (parse-bytes "\"string literal\\xAB\\xCD\\xFF\"")
  
  
  )
