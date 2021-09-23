#lang racket/base
(require racket/match
         "record.rkt"
         "monad.rkt")

; class Monad m => ReadByte
; read-byte :: m byte
(record ReadByte (Monad read-byte))

; ReadByte => m byte
(define (read-opcode rb)
  (match-define (ReadByte read-byte) rb)
  read-byte)

; ReadByte => m uint8
(define (read-uint8 rb)
  (match-define (ReadByte read-byte) rb)
  read-byte)

; ReadByte => m int16
(define (read-int16 rb)
  (match-define (ReadByte [Monad (Monad unit >>= >>)] read-byte) rb)
  (>>= read-byte
       (λ (bu)
         (>>= read-byte
              (λ (bl)
                (let ([x (+ (* 256 bu) bl)])
                  (unit (if (< x (expt 2 15))
                          x
                          (- x (expt 2 16))))))))))


; ReadByte => m varuint
(define (read-varuint rb)
  (match-define (ReadByte [Monad (Monad unit >>= >>)] read-byte) rb)
  (>>= read-byte
       (λ (b)
         (if (zero? (bitwise-and b #x80))
           (unit b)
           (>>= (read-varuint rb) (λ (n) (unit (+ (bitwise-and b #x7F) (* n #x80)))))))))

; ReadByte => m bytes
(define (read-bytes rb)
  (match-define (ReadByte [Monad (Monad unit >>= >>)] read-byte) rb)
  (>>= (>>= (read-varuint rb)
            (λ (n)
              (let loop ([n n])
                (if (zero? n)
                  (unit (list))
                  (>>= read-byte
                       (λ (b)
                         (>>= (loop (sub1 n))
                              (λ (bs) (unit (cons b bs))))))))))
       (λ (bs) (unit (apply bytes bs)))))

(provide ReadByte
         read-opcode
         read-uint8
         read-int16
         read-varuint
         read-bytes)