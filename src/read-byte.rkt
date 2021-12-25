#lang racket/base
(require racket/match
         "static/object.rkt"
         #;
         "monad.rkt")

(define read-byte-extras
  (inc (read-byte
        unit >>=)
       [read-opcode
        read-byte]
       [read-uint8
        read-byte]
       [read-int16
        (>>= read-byte
             (λ (bu)
               (>>= read-byte
                    (λ (bl)
                      (let ([x (+ (* 256 bu) bl)])
                        (unit (if (< x (expt 2 15))
                                x
                                (- x (expt 2 16)))))))))]
       [read-varuint
        (>>= read-byte
             (λ (b)
               (if (zero? (bitwise-and b #x80))
                 (unit b)
                 (>>= read-varuint
                      (λ (n) (unit (+ (bitwise-and b #x7F) (* n #x80))))))))]
       [read-bytes
        (>>= (>>= read-varuint
                  (λ (n)
                    (let loop ([n n])
                      (if (zero? n)
                        (unit (list))
                        (>>= read-byte
                             (λ (b)
                               (>>= (loop (sub1 n))
                                    (λ (bs) (unit (cons b bs))))))))))
             (λ (bs) (unit (apply bytes bs))))]
       [read-intcblock
        (>>= read-varuint
             (λ (n)
               (let loop ([n n])
                 (if (zero? n)
                   (unit (list))
                   (>>= read-varuint
                        (λ (x)
                          (>>= (loop (sub1 n))
                               (λ (xs) (unit (cons x xs))))))))))]
       [read-bytecblock
        (>>= read-varuint
             (λ (n)
               (let loop ([n n])
                 (if (zero? n)
                   (unit (list))
                   (>>= read-bytes
                        (λ (bs)
                          (>>= (loop (sub1 n))
                               (λ (bss) (unit (cons bs bss))))))))))]
       [read-offset
        read-int16]))

(provide read-byte-extras)
