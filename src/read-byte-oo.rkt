#lang racket/base
(require racket/match
         #;"static/record.rkt"
         #;"monad.rkt"
         )


; the two types are the self and super
; enforce that kind of ordering though?
; what about mutually-recursive mixins?
; the super is what it assumes exists,
; the self is what it provides, but that
; does not need to be a superset of the
; super.

(λ (self super)
  (λ (msg)
    (case msg
      [(read-opcode)
       (let ([read-byte (self 'read-byte)])
         read-byte)]
      [(read-uint8)
       (let ([read-byte (self 'read-byte)])
         read-byte)]
      [else
       (super msg)])))

(mix Read ReadByte
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
      (>>= (>>= (read-varuint rb)
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
                             (λ (bss) (unit (cons bs bss))))))))))])

(fix Read)


(mixin)

(define read
  (λ (self super)
    (λ (msg)
      (case msg
        [(read-uint8)
         ((self self super) 'read-byte)]
        [(read-int16)
         (let ([>>= ((self self super) '>>=)]
               [read-byte ((self self super) 'read-byte)])
           (>>= read-byte
                (λ (bu)
                  (let ([>>= ((self self super) '>>=)]
                        [read-byte ((self self super) 'read-byte)])
                    (>>= read-byte
                         (λ (bl)
                           (let ([unit ((self self super) 'unit)])
                             (let ([x (+ (* 256 bu) bl)])
                               (unit (if (< x (expt 2 15))
                                       x
                                       (- x (expt 2 16))))))))))))]
        []))))

(λ (self super)
  (λ (msg)
    (case msg
      [(unit)
        ...]
      [(>>=) 
       ...]
      [(read-byte) 
       ...]
      [(read-uint8) 
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
      [else
       (error 'no-message)])))


#|
; class Monad m => ReadByte
; read-byte :: m byte
(record ReadByte Monad (read-byte)
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
         (>>= (>>= (read-varuint rb)
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
                                (λ (bss) (unit (cons bs bss))))))))))])

(define read-byte-object-thing
  (λ (rb)
    (λ (self)
      )))

; ReadByte => m bytes
(define (read-bytes rb)
  (match-define (ReadByte [monad (Monad unit >>=)] read-byte) rb)
  )

(define (read-intcblock rb)
  (match-define (ReadByte [monad (Monad unit >>=)] read-byte) rb)
  )

(define (read-bytecblock rb)
  (match-define (ReadByte [monad (Monad unit >>=)] read-byte) rb)
  )

(provide ReadByte
         read-opcode
         read-uint8
         read-int16
         read-varuint
         read-bytes
         read-intcblock
         read-bytecblock)
|#
