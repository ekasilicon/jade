#lang racket/base
(require racket/match
         "base.rkt")

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
      (lift hex-digits→numeral (p+ (cc hex-digit?)))))

(module+ test
  (parse-success hex-varuint
                 "0x123"
                 291))

(define octal-digits→numeral (make-digits→numeral 8))

(define octal-varuint
  (>> (literal "0")
      (lift octal-digits→numeral (p+ (cc octal-digit?)))))

(module+ test
  (parse-success octal-varuint
                 "0123"
                 83))

(define decimal-digits→numeral (make-digits→numeral 10))

(define decimal-varuint
  (lift decimal-digits→numeral (p+ (cc decimal-digit?))))

(module+ test
  (parse-success decimal-varuint
                 "123"
                 123))

(define varuint
  (∨ hex-varuint
     octal-varuint
     decimal-varuint))

(module+ test
  (parse-success varuint
                 "0x123"
                 291)

  (parse-success varuint
                 "0123"
                 83)

  (parse-success varuint
                 "123"
                 123))

(define guarded-varuint
  (guard varuint
         "a nonnegative integer like 1234..."
         "a nonnegative integer like 01234..."
         "a nonnegative integer like 0x1234..."))

(module+ test
  (parse-failure guarded-varuint
                 "hello"
                 #rx"a nonnegative integer"))

(define uint8
  (>>= varuint
       (λ (x)
         (if (< x 256)
           (unit x)
           fail))))

(define guarded-uint8
  (guard uint8 "a nonnegative integer which can fit in a byte"))

(provide (all-defined-out))

(module+ test
  (parse-failure (>>= guarded-uint8 (λ (x) (>> end-of-input (unit x))))
                 "256"
                 #rx"fit in a byte"))

(define (parse-varuint input)
  ((>> whitespace*
       (>>0 guarded-varuint
            whitespace*
            end-of-input))
   input 0
   (λ (xs _₀ _₁) (match-let ([(list x) xs]) x))
   void)
  #;
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
  #;
  (varuint input 0
           (λ (x i fk)
             (end-of-input input i
                           (λ (_ i fk) (match-let ([(list x) x]) x))
                           fail))
           fail))

(provide parse-varuint)

(module+ test
  (parse-varuint "0x123")
  (parse-varuint "0123")
  (parse-varuint "123")
  #;(parse-varuint "hello")
  (parse-varuint "123 ")
  (parse-varuint " 123")
  (parse-varuint " 123 "))

