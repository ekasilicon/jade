#lang racket/base
(require racket/match
         net/base64
         "../src/static/object.rkt"
         "../src/instruction/read.rkt"
         "../src/read-byte.rkt"
         "../src/prefix.rkt"
         "../src/monad.rkt")

; a simple disassembler
(define (disassemble b64-encoded-bytecode)
  (match-let* ([(cons lsv bytecode)
                (read-prefix (base64-decode b64-encoded-bytecode))]
               [read-byte
                (inc ()
                     [unit
                       (λ xs (λ (i) (cons xs i)))]
                     [>>=
                      (λ (m f)
                        (λ (i)
                          (match (m i)
                            [(cons xs i)
                             ((apply f xs) i)]
                            [#f #f])))]
                     [read-byte
                      (λ (i)
                        (and (< i (bytes-length bytecode))
                             (cons (list (bytes-ref bytecode i)) (add1 i))))])]
               [read-instruction ((fix (mix (instruction-read/version lsv)
                                            read-byte-extras
                                            monad-extras
                                            read-byte))
                                  'read-instruction)])
    (values lsv
            (let loop ([pc 0]
                       [assembly (hasheqv)])
              (match (read-instruction pc)
                [(cons (list instr) succ-pc)
                 (loop succ-pc
                       (hash-set assembly pc (cons instr succ-pc)))]
                [#f
                 (hash-set assembly pc (list))])))))

(provide disassemble)
