#lang racket/base
(require (only-in racket/match match-lambda)
         (only-in racket/string string-join)
         (only-in net/base64 base64-encode)
         "../static/sumtype.rkt"
         "../static/object.rkt"
         "../assembly.rkt"
         "../instruction/show.rkt")

(define assembly-show
  (match-lambda
    [(assembly [logic-sig-version lsv] directives)
     (define-values (instruction-show label-show uint-show bytes-show)
       (let ([show (fix (mix (instruction-show/version lsv)
                             (inc ()
                                  [label-show
                                   symbol->string]
                                  [uint8-show
                                   number->string]
                                  [uint-show
                                   number->string]
                                  [uints-show
                                   (λ (ns) (string-join (map (self 'uint-show) ns) " "))]
                                  [bytes-show
                                   (λ (bs) (string-append "base64(" (bytes->string/utf-8 (base64-encode bs #"")) ")"))]
                                  [bytess-show
                                   (λ (bss) (string-join (map (self 'bytes-show) bss) " "))]
                                  [labels-show
                                   (λ (ℓs) (string-join (map (self 'label-show) ℓs) " "))])))])
         (values (show 'instruction-show)
                 (show 'label-show)
                 (show 'uint-show)
                 (show 'bytes-show))))
     (define directive-show
       (sumtype-case-lambda Directive
         [(pragma content)
          (string-append "#pragma " content)]
         [(label ℓ)
          (string-append (label-show ℓ) ":")]
         [(Pseudoinstruction pinstr)
          (string-append
           "  "
           (sumtype-case Pseudoinstruction pinstr
             [(instruction [instruction instr])
              (instruction-show instr)]
             [(varuint-immediate value)
              (string-append "int " (uint-show value))]
             [(bytes-immediate value)
              (string-append "bytes " (bytes-show value))]))]))
     (string-append
      (format "#pragma version ~a\n" lsv)
      (string-join (map directive-show directives) "\n"))]))

(provide assembly-show)

(module+ main
  (require "../instruction.rkt")

  (displayln
   (assembly-show
    (assembly
     [logic-sig-version 3]
     [directives
      (list (instruction [instruction (err)])
            (instruction [instruction (sha256)])
            (instruction [instruction (bnz [offset 'label1])])
            (instruction [instruction (==)])
            (varuint-immediate [value 25])
            (label [ℓ 'label1])
            (bytes-immediate [value #"hello"]))]))))
