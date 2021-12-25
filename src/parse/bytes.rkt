#lang racket/base
(require (prefix-in rkt: (only-in racket/base bytes))
         racket/match
         "base.rkt"
         "number.rkt")

(define ((decode-baseXX digit-values value-width) cs)
  (apply rkt:bytes
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

(define base64-bytes (lift base64-cs->bytes (p* (cc base64-digit?))))

(define base32-digit-values
  (for/hasheqv ([c (in-string "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")]
                [i (in-naturals)])
    (values c i)))

(define (base32-digit? c)
  (hash-has-key? base32-digit-values c))

(define base32-cs->bytes (decode-baseXX base32-digit-values 5))

(define base32-bytes (lift base32-cs->bytes (p* (cc base32-digit?))))

(define ((negate f) x) (not (f x)))

(define bytes-hex-literal
  (>> (literal "0x")
      (>>= (p* (lift hex-digits→numeral (read-chars 2 hex-digit?)))
           (λ (bs) (unit (apply rkt:bytes bs))))))

(module+ test
  (parse-success bytes-hex-literal
                 "0x3132333435"
                 #"12345")

  (parse-failure (>>0 bytes-hex-literal end-of-input)
                 "0x313233343"
                 #rx""))

(define bytes-string-literal
  (>> (c #\")
      (>>= (let loop ()
             (>>= read-char
                  (match-lambda
                    [#\" (unit (list))]
                    [#\\ (>>= (∨ (>>= read-char
                                      (match-lambda
                                        [#\n (unit (char->integer #\newline))]
                                        [#\r (unit (char->integer #\return))]
                                        [#\t (unit (char->integer #\tab))]
                                        [#\\ (unit (char->integer #\\))]
                                        [#\" (unit (char->integer #\"))]
                                        [#\x (lift hex-digits→numeral (read-chars 2 hex-digit?))]
                                        [_ fail]))
                                 (lift octal-digits→numeral (read-chars 3 octal-digit?)))
                              (λ (b) (>>= (loop) (λ (bs) (unit (cons b bs))))))]
                    [c (let ([b (char->integer c)])
                         (if (< b 256)
                           (>>= (loop) (λ (bs) (unit (cons b bs))))
                           fail))])))
           (λ (bs) (unit (apply rkt:bytes bs))))))


(define bytes
  (∨ (>> (∨ (literal "base64")
            (literal "b64"))
         whitespace+
         base64-bytes)
     (>> (∨ (literal "base64")
            (literal "b64"))
         (literal "(")
         (>>0 base64-bytes
              (literal ")")))
     (>> (∨ (literal "base32")
            (literal "b32"))
         whitespace+
         base32-bytes)
     (>> (∨ (literal "base32")
            (literal "b32"))
         (literal "(")
         (>>0 base32-bytes
              (literal ")")))
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

(provide (all-defined-out))

(define (parse-bytes input)
  ((>> whitespace*
       (>>0 guarded-bytes
            whitespace*
            end-of-input))
   input 0
   (λ (xs _₀ _₁) (match-let ([(list x) xs]) x))
   void))

(provide parse-bytes)

(module+ test
  (parse-bytes "0x3132333435")
  (parse-bytes "b64 abcde")
  #;(parse-bytes "b65 abcde")
  (parse-bytes "\"hello\"")
  #;(parse-bytes "0x313233343")
  )

#;
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
  (bytesp input 0
          (λ (x i fk)
            (end-of-input input i
                          (λ (_ i fk) (match-let ([(list x) x]) x))
                          fail))
          fail))
