#lang racket/base
(require (prefix-in rkt: (only-in racket/base bytes))
         racket/match
         "base.rkt"
         "uint.rkt")

(define ((decode-baseXX digit-values value-width) cs)
  (list->bytes
   (let loop ([num-bits 0]
              [accumulator 0]
              [cs cs])
     (if (< num-bits 8)
       (match cs
         [(list)
          #;
          (list (arithmetic-shift accumulator (- 8 num-bits)))
          ; if there are no more characters, the remaining bits are discarded
          (list)]
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

(define (baseXX-sequence baseXX-digit? multiple)
  (let loop ([i 0])
    (∨ (fmap cons (?c baseXX-digit?) (delay (loop (add1 i))))
       (>>0 (unit (list))
            (∨ (litp (make-string (remainder (- multiple (remainder i multiple)) multiple) #\=))
               (unit))))))

(define base64-digit-values
  (for/hasheqv ([c (in-string "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")]
                [i (in-naturals)])
    (values c i)))

(define (base64-digit? c)
  (hash-has-key? base64-digit-values c))

(define base64-bytes
  (fmap (decode-baseXX base64-digit-values 6)
        (baseXX-sequence base64-digit? 4)))

(module+ test
  (parse-success base64-bytes
                 "aGVsbG8="
                 #"hello"))

(define base32-digit-values
  (for/hasheqv ([c (in-string "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")]
                [i (in-naturals)])
    (values c i)))

(define (base32-digit? c)
  (hash-has-key? base32-digit-values c))

(define base32-bytes
  (fmap (decode-baseXX base64-digit-values 5)
        (baseXX-sequence base32-digit? 8)))

(define bytes-hex-literal
  (>> (litp "0x")
      (fmap list->bytes (⋆p (fmap hex-digits→numeral (np 2 hex-digit))))))

(module+ test
  (parse-success bytes-hex-literal
                 "0x3132333435"
                 #"12345")

  (parse-failure bytes-hex-literal
                 "0x313233343"
                 #rx""))

(define bytes-string-literal
  (>> (cp #\")
      (fmap
       list->bytes
       (let loop ()
         (∨ (>> (cp #\") (unit (list)))
            (fmap cons
                  (>>= charp
                       (match-lambda
                         [#\\ (∨ (>>= charp
                                      (match-lambda
                                        [#\n (unit (char->integer #\newline))]
                                        [#\r (unit (char->integer #\return))]
                                        [#\t (unit (char->integer #\tab))]
                                        [#\\ (unit (char->integer #\\))]
                                        [#\" (unit (char->integer #\"))]
                                        [#\x (fmap hex-digits→numeral (np 2 hex-digit))]
                                        [_ fail]))
                                 (fmap octal-digits→numeral (np 3 octal-digit)))]
                         [c (let ([b (char->integer c)])
                              (if (< b 256) (unit b) fail))]))
                  (delay (loop))))))))


(define bytes
  (∨ (>> (∨ (litp "base64")
            (litp "b64"))
         space+
         base64-bytes)
     (>> (∨ (litp "base64")
            (litp "b64"))
         (litp "(")
         (>>0 base64-bytes
              (litp ")")))
     (>> (∨ (litp "base32")
            (litp "b32"))
         space+
         base32-bytes)
     (>> (∨ (litp "base32")
            (litp "b32"))
         (litp "(")
         (>>0 base32-bytes
              (litp ")")))
     bytes-hex-literal
     bytes-string-literal))

(define guarded-bytes
  (guard bytes
         "base64 AAAA..."
         "b64 AAAA..."
         "base64(AAAA...)"
         "b64(AAAA...)"
         "base32 AAAA..."
         "b32 AAAA..."
         "base32(AAAA...)"
         "b32(AAAA...)"
         "0x0123456789abcdef..."
         "\"string literal\x01\x02\""))

(provide guarded-bytes)

(define (parse-bytes input)
  (match-let ([(list x) (parse (>> space* (>>0 guarded-bytes space*))
                               input)])
    x))

(provide parse-bytes)

(module+ test
  (parse-success guarded-bytes
                 "0x3132333435"
                 #"12345")
  (parse-success guarded-bytes
                 "b64 aGVsbG8"
                 #"hello")
  (parse-failure guarded-bytes
                 "b65 aGVsbG8"
                 #rx"")
  (parse-success guarded-bytes
                 "\"hello\""
                 #"hello")

  (require rackunit)

  (check-equal? (parse-bytes "b64(aGVsbG8)")
                #"hello")
  
  (with-handlers ([exn:fail? void])
    (parse-bytes "b64(aGVsbG8) 123")) )

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
