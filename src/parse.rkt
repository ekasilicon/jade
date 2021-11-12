#lang racket/base
(require racket/match
         racket/string
         racket/pretty
         "record.rkt"
         "sumtype.rkt"
         (prefix-in i: "instruction.rkt"))

; Need to add cut so that committed failure can be represented
; and better errors can be reported.

(define ((unit . xs) input i sk fk) (sk xs i fk))
(define (fail input i sk fk) (fk))

(define ((>>= m f) input i sk fk)
  (m input i (λ (xs i fk) ((apply f xs) input i sk fk)) fk))

(define (>> m . ms)
  (foldl (λ (m m₀) (>>= m₀ (λ _ m))) m ms))

(define ((lift f) . xs)
  (call-with-values (λ () (apply f xs)) unit))

(define ((∨ . ps) input i sk fk) 
  (let loop ([ps ps])
    (match ps
      [(list)
       (fk)]
      [(cons p ps)
       (p input i sk (λ () (loop ps)))])))

(define (∘ . ps)
  (let loop ([ps ps])
    (match ps
      [(list)
       (unit)]
      [(cons p ps)
       (>>= p (λ (x) (>>= (loop ps) (λ xs (apply unit x xs)))))])))

(define ((p* p) input i sk fk) 
  (let loop ([xs (list)]
             [i i]
             [fk fk])
    (p input i (λ (x i fk)
                 (match x
                   [(list x)
                    (loop (cons x xs) i fk)]
                   [_
                    (error 'p* "expected a single result")]) )
       (λ () (sk (list (reverse xs)) i fk)))))

(define (p+ p)
  (>>= p (λ (x) (>>= (p* p) (λ (xs) (unit (cons x xs)))))))

(define (p? p)
  (∨ p (unit #f)))

(define (end-of-input input i sk fk)
  (if (= (string-length input) i)
    (sk (list #f) i fk)
    (fk)))

(define (read-char input i sk fk)
  (if (= (string-length input) i)
    (fk)
    (sk (list (string-ref input i)) (add1 i) fk)))

(define (read-chars n ?)
  (if (zero? n)
    (unit (list))
    (>>= read-char
         (λ (c)
           (if (? c)
             (>>= (read-chars (sub1 n) ?)
                  (λ (cs) (unit (cons c cs))))
             fail)))))

(define (c c₀)
  (>>= read-char
       (λ (c)
         (if (eqv? c₀ c)
           (unit c)
           fail))))

(define (cc ?)
  (>>= read-char
       (λ (c)
         (if (? c)
           (unit c)
           fail))))

(define (^ s)
  (let ([cs (string->list s)])
    (λ (c) (not (memv c cs)))))

(define (^^ s)
  (let ([cs (string->list s)])
    (λ (c) (memv c cs))))

(define (literal s)
  (let loop ([cs (string->list s)])
    (match cs
      [(list)
       (unit s)]
      [(cons c₀ cs)
       (>> (c c₀) (loop cs))])))

(define space* (p* (c #\space)))
(define space+ (p+ (c #\space)))

(define whitespace* (p* (∨ (c #\space) (c #\tab))))
(define whitespace+ (p+ (∨ (c #\space) (c #\tab))))

(define (snippet input i)
  (substring input i (min (string-length input) (+ i 24))))

(define (report input i expected . expecteds)
  (format #<<REPORT
parse failure

  at position ~a
  encountered ~a
  
but expected~a

~a
REPORT
          i
          (let ([s (format "~s" (snippet input i))])
            (substring s 1 (max 1 (sub1 (string-length s)))))
          (if (null? expecteds) "" " one of")
          (string-join (map (λ (expected) (string-append "  " expected "\n"))
                            (cons expected expecteds))
                       "")))

(define ((guard p expected . expecteds) input i sk _)
  (p input i sk (λ () (error (apply report input i expected expecteds)))))


(define ((trace which p) input i sk fk)
  (displayln 'TRACE)
  (displayln which)
  (println p)
  (println i)
  (println (snippet input i))
  (p input i (λ (x i fk)
               (println x)
               (sk x i fk))
     (λ ()
       (displayln "FAILED")
       (fk))))

; numbers

(define digit-values
  (hasheqv #\0 0
           #\1 1
           #\2 2
           #\3 3
           #\4 4
           #\5 5
           #\6 6
           #\7 7
           #\8 8
           #\9 9
           #\a 10 #\A 10
           #\b 11 #\B 11
           #\c 12 #\C 12
           #\d 13 #\D 13
           #\e 14 #\E 14
           #\f 15 #\F 15))

(define octal-digit? (^^ "01234567"))
(define decimal-digit? (^^ "0123456789"))
(define hex-digit? (^^ "0123456789abcdefABCDEF"))

(define ((make-digits→numeral radix) ds)
  (foldl (λ (d n) (+ (* n radix) (hash-ref digit-values d))) 0 ds))

(define hex-digits→numeral (make-digits→numeral 16))

(define hex-varuint
  (>> (literal "0x")
      (>>= (p+ (cc hex-digit?)) (lift hex-digits→numeral))))

(define octal-digits→numeral (make-digits→numeral 8))

(define octal-varuint
  (>> (literal "0")
      (>>= (p+ (cc octal-digit?)) (lift octal-digits→numeral))))

(define decimal-digits→numeral (make-digits→numeral 10))

(define decimal-varuint
  (>>= (p+ (cc decimal-digit?)) (lift decimal-digits→numeral)))

(define varuint
  (∨ hex-varuint
     octal-varuint
     decimal-varuint))

(define guarded-varuint
  (guard varuint "a nonnegative integer"))

(define uint8
  (>>= varuint
       (λ (x)
         (if (< x 256)
           (unit x)
           fail))))

(define guarded-uint8
  (guard uint8 "a nonnegative integer which can fit in a byte"))

(define (parse-varuint input)
  (define (fail)
    (error (format #<<MESSAGE
expected a nonnegative integer of the form

  1234...
  01234...
  0x1234...

but got

  ~s

MESSAGE
                   input)))
  (varuint input 0
           (λ (x i fk)
             (end-of-input input i
                           (λ (_ i fk) (match-let ([(list x) x]) x))
                           fail))
           fail))

(provide parse-varuint)

; bytes

(define ((decode-baseXX digit-values value-width) cs)
  (apply bytes
         (let loop ([num-bits 0]
                    [accumulator 0]
                    [cs cs])
           (if (< num-bits 8)
             (match cs
               [(list)
                (list (arithmetic-shift accumulator (- 8 num-bits)))]
               [(cons c cs)
                (loop (+ num-bits value-width)
                      (bitwise-ior (arithmetic-shift accumulator value-width)
                                   (hash-ref digit-values c))
                      cs)])
             (cons (arithmetic-shift accumulator
                                     (- 8 num-bits))
                   (loop (- num-bits 8)
                         (bitwise-and accumulator
                                      (sub1 (arithmetic-shift 1 (- num-bits 8))))
                         cs))))))

(define base64-digit-values
  (for/hasheqv ([c (in-string "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")]
                [i (in-naturals)])
    (values c i)))

(define (base64-digit? c)
  (hash-has-key? base64-digit-values c))

(define base64-cs->bytes (decode-baseXX base64-digit-values 6))

(define base32-digit-values
  (for/hasheqv ([c (in-string "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")]
                [i (in-naturals)])
    (values c i)))

(define (base32-digit? c)
  (hash-has-key? base32-digit-values c))

(define base32-cs->bytes (decode-baseXX base32-digit-values 5))

(define ((negate f) x) (not (f x)))

(define hex-literal
  (>> (literal "0x")
      (>>= (let loop ()
             (∨ (>> (∨ end-of-input (cc (negate hex-digit?)))
                    (unit (list)))
                (>>= (>>= (read-chars 2 hex-digit?)
                          (lift hex-digits→numeral))
                     (λ (b) (>>= (loop) (λ (bs) (unit (cons b bs))))))))
           (λ (bs) (unit (apply bytes bs))))))

(define string-literal
  (>> (c #\")
      (>>= (let loop ()
             (>>= read-char
                  (match-lambda
                    [#\" (unit (list))]
                    [#\\ (>>= (>>= read-char
                                   (match-lambda
                                     [#\n (unit (char->integer #\newline))]
                                     [#\r (unit (char->integer #\return))]
                                     [#\t (unit (char->integer #\tab))]
                                     [#\\ (unit (char->integer #\\))]
                                     [#\" (unit (char->integer #\"))]
                                     [#\0 (>>= (read-chars 3 octal-digit?)
                                               (lift (make-digits→numeral 8)))]
                                     [#\x (>>= (read-chars 2 hex-digit?)
                                               (lift (make-digits→numeral 16)))]
                                     [_ fail]))
                              (λ (b) (>>= (loop) (λ (bs) (unit (cons b bs))))))]
                    [c (let ([b (char->integer c)])
                         (if (< b 256)
                           (>>= (loop) (λ (bs) (unit (cons b bs))))
                           fail))])))
           (λ (bs) (unit (apply bytes bs))))))

(define pbytes
  (∨ (>> (∨ (literal "base64")
            (literal "b64"))
         whitespace+
         (>>= (p* (cc base64-digit?)) (lift base64-cs->bytes)))
     (>> (∨ (literal "base64")
            (literal "b64"))
         (literal "(")
         (>>= (p* (cc base64-digit?))
              (λ (base64-cs)
                (>> (literal ")")
                    (unit (base64-cs->bytes base64-cs))))))
     (>> (∨ (literal "base32")
            (literal "b32"))
         whitespace+
         (>>= (p* (cc base32-digit?)) (lift base32-cs->bytes)))
     (>> (∨ (literal "base32")
            (literal "b32"))
         (literal "(")
         (>>= (p* (cc base32-digit?))
              (λ (base32-cs)
                (>> (literal ")")
                    (unit (base32-cs->bytes base32-cs))))))
     hex-literal
     string-literal))

(provide parse-bytes)

(define guarded-pbytes
  (guard pbytes
         "bytes as base64 ..."
         "bytes as base32 ..."
         "bytes as 0x..."
         "bytes as a string"))

#|
bytes
byte[+ ]base64[+ ](base64)
byte[+ ]b64[+ ](base64)
byte[+ ]base64\((base64)\)
byte[+ ]b64\((base64)\)
byte[+ ]base32[+ ](base32)
byte[+ ]b32[+ ](base32)
byte[+ ]base32\((base32)\)
byte[+ ]b32\((base32)\)
byte[+ ](hex-number)
byte[+ ](string)
|#

(define (parse-bytes input)
  (define (fail)
    (error (format #<<MESSAGE
expected bytes of the form

  base64 AAAA...
  b64 AAAA...
  base64(AAAA...)
  b64(AAAA...)
  base32 AAAA...
  b32 AAAA...
  base32(AAAA...)
  b32(AAAA...)
  0x0123456789abcdef...
  "string literal\x01\x02"

but got

  ~s

MESSAGE
                   input)))
  (pbytes input 0
          (λ (x i fk)
            (end-of-input input i
                          (λ (_ i fk) (match-let ([(list x) x]) x))
                          fail))
          fail))

(define (make-instruction name constructor . arguments)
  (>> (literal name)
      (>>= (let loop ([args arguments])
             (match args
               [(list)
                (unit (list))]
               [(cons arg args)
                (>> whitespace+
                    (>>= arg
                         (λ (x)
                           (>>= (loop args)
                                (λ (xs) (unit (cons x xs)))))))]))
           (λ (xs) (unit (apply constructor xs))))))

(require (for-syntax racket/base
                     racket/match
                     syntax/parse))

(define-syntax enumtype-parser
  (syntax-parser
    [(_ typename:id)
     (match (syntax-local-value #'typename (λ () #f))
       [(sumtype-info variants)
        (with-syntax ([(varname ...) (map symbol->string (map syntax->datum variants))]
                      [(variant ...) variants])
          #'(guard (∨ (>> (literal varname) (unit (variant))) ...)
                   varname ...) )]
       [_ (raise-syntax-error #f "not a sumtype" #'typename)])]))

(define transaction-field
  (enumtype-parser i:TransactionField))

(define global-field
  (enumtype-parser i:GlobalField))

(define label-identifier
  (>>= (∘ (cc (^^ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-"))
          (p* (cc (^^ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-")))) 
       (lift (λ (c cs) (string->symbol (apply string c cs))))))

(define guarded-label
  (guard label-identifier
          "a label"))

(define asset-holding-field
  (enumtype-parser i:AssetHoldingField))

(define asset-params-field
  (enumtype-parser i:AssetParamsField))

(define app-params-field
  (enumtype-parser i:AppParamsField))

(define-sumtype Pseudoinstruction
  i:Instruction
  (int uint)
  (byte bytes))

(define-syntax instruction-parser
  (syntax-parser
    [_
     (match-let ([(sumtype-info variants) (syntax-local-value #'Pseudoinstruction)])
       (with-syntax ([(parser ...)
                      (map
                       (λ (variant)
                         (match-let ([(record-info fields _ constructor _ _ _) (syntax-local-value variant)])
                           (with-syntax ([name (symbol->string (syntax->datum variant))]
                                         [constructor constructor]
                                         [(field ...) (map
                                                        (λ (field)
                                                          (match field
                                                            ['v #'guarded-uint8]
                                                            ['i #'guarded-uint8]
                                                            ['n #'guarded-uint8]
                                                            ['uints #'(p* (>> whitespace* varuint))]
                                                            ['bytess #'(p* (>> whitespace* pbytes))]
                                                            ['group-index #'guarded-uint8]
                                                            ['array-index #'guarded-uint8]
                                                            ['offset #'guarded-label]
                                                            ['start #'guarded-uint8]
                                                            ['end #'guarded-uint8]
                                                            ['length #'guarded-uint8]
                                                            ['bytes #'guarded-pbytes]
                                                            ['uint #'guarded-varuint]
                                                            ['field
                                                             (match (syntax->datum variant)
                                                               [(or 'txn 'gtxn 'txna 'gtxna 'gtxns 'gtxnsa 'itxn_field 'itxn 'itxna 'txnas 'gtxnas 'gtxnsas)
                                                                #'transaction-field]
                                                               ['global #'global-field]
                                                               ['asset_holding_get #'asset-holding-field]
                                                               ['asset_params_get #'asset-params-field]
                                                               ['app_params_get #'app-params-field])]
                                                            ))
                                                        fields)])
                             #'(make-instruction name constructor field ...))))
                       variants)])
         #'(∨ parser ...)))]))

(define instruction
  (>> whitespace*
      (instruction-parser)))

(define pragma-directive
  (>> (literal "#pragma ")
      (>>= (p* (cc (^ "\r\n")))
           (lift (λ (cs) (pragma [content (apply string cs)]))))))

(define label-declaration
  (>>= label-identifier
       (λ (ℓ)
         (>> space*
             (literal ":")
             (unit (label ℓ))))))

(define directive
  (∨ instruction
     pragma-directive
     label-declaration))

(define-sumtype Directive
  Pseudoinstruction
  (pragma content)
  (label ℓ))

(define comment
  (>> (literal "//")
      (>>= (p* (cc (^ "\r\n"))) (lift (λ (cs) (apply string cs))))))

(define newline
  (>>= read-char
       (λ (c)
         (if (eqv? c #\newline)
           (unit #f)
           (if (eqv? c #\return)
             (>>= read-char
                  (λ (c)
                    (if (eqv? c #\newline)
                      (unit #f)
                      fail)))
             fail)))))

(define line
  (>>= (p? directive)
       (λ (v)
         (>> whitespace*
             (p? comment)
             (∨ newline end-of-input)
             (unit v)))))

(define (parse input)
  (let loop ([i 0])
    (end-of-input input i (λ (_ i fk) (list))
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
                            (error 'parse (report input i
                                                  "an instruction"
                                                  "a comment"
                                                  "a pragma directive"))))))))

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
  (parse-bytes "\"abc\\n\\r\\t\\0377\"")
  (parse-bytes "\"abc\\n\\r\\t\\x10\"")
  (parse-bytes "\"string literal\\xAB\\xCD\\xFF\"")
  
  #;
  (let ([input (port->string (current-input-port))])
    (let ([directives (time (parse input))])
      (pretty-print directives)
      (pretty-print (resolve-control-flow directives)))))
