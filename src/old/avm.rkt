#lang racket/base
(require "static/object.rkt"
         "static/sumtype.rkt"
         "monad.rkt"
         "read-byte.rkt"
         "vm.rkt"
         (prefix-in i: "instruction.rkt"))

(define-sumtype Result
  (underway values ς)
  (returned code)
  (failure! message))

(define concrete
  (mix (vm/version 3)
       [panic
           (λ (template . args)
             (failure! (apply format template args)))]
       read-byte-extras
       [read-byte
        (>>= (get bytecode)
             (λ (bc)
               (>>= (get pc)
                    (λ (pc)
                      (if (>= pc (bytes-length bc))
                        (panic "attempt to read at ~a but bytecode ends at ~a" pc (bytes-length bc))
                        (>> (update pc add1)
                            (unit (bytes-ref bc pc))))))))]
       monad-extras
       [unit
         (λ values (λ (ς) (underway values ς)))]
       [>>=
        (λ (m f)
          (λ (ς)
            (sumtype-case Result (m ς)
              [(underway [values xs] ς)
               ((apply f xs) ς)]
              #:otherwise values)))]))

(define (execute program environment)
  ; requires JSON package
  ; requires transaction parameters
  ; executes appropriate program with parameters
  ; reports the effects of execution (global/local storage writes)
  ; has four distinct notions of failure:
  ; 1. AVM failure---failure when the AVM would fail
  ; 2. environment failure---failure because not all information is known that must be known
  ;    this kind of failure can be remedied by the user providing more information about the
  ;    transaction execution environment.
  ; 3. VM failure--failure because the VM doesn't simulate a certain aspect of the AVM.
  ;    these are known deficiencies (else how would the appropriate kind of error be thrown?)
  ;    and should be rare.
  ; 4. dynamic failure---failure because of a coding error
  ;    these too should be rare.
  )
