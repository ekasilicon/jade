#lang racket/unit
(require "read-sig.rkt"
         "read-encoded-sig.rkt"
         "monad-sig.rkt")

(import read^ monad^)
(export read-encoded^)

(define read-opcode read-byte)
(define read-uint8 read-byte)
(define read-int16
  (>>= read-byte
       (λ (bu)
         (>>= read-byte
              (λ (bl)
                (let ([x (+ (* 256 bu) bl)])
                  (unit (if (< x (expt 2 15))
                          x
                          (- x (exp 2 16))))))))))

(define read-varuint
  (>>= read-byte
       (λ (b)
         (if (zero? (bitwise-and b #x80))
           (unit b)
           (>>= read-varuint (λ (n) (unit (+ (bitwise-and b #x7F) (* n 128)))))))))

(define read-bytes
  (>>= (>>= read-varuint
            (λ (n)
              (let loop ([n n])
                (if (zero? n)
                  (unit (list))
                  (>>= read-byte
                       (λ (b)
                         (>>= (loop (sub1 n))
                              (λ (bs) (unit (cons b bs))))))))))
       (λ (bs) (unit (apply bytes bs)))))

(define read-intcblock
  (>>= read-varuint
       (λ (n)
         (let loop ([n n])
           (if (zero? n)
             (unit (list))
             (>>= read-varuint
                  (λ (x)
                    (>>= (loop (sub1 n))
                         (λ (xs)
                           (unit (cons x xs)))))))))))

(define read-bytecblock
  (>>= read-varuint
       (λ (n)
         (let loop ([n n])
           (if (zero? n)
             (unit (list))
             (>>= read-bytes
                  (λ (x)
                    (>>= (loop (sub1 n))
                         (λ (xs) (unit (cons x xs)))))))))))
