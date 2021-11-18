#lang racket/base
(require racket/match
         "static/record.rkt"
         "monad.rkt")

; class Monad monad => LogicSigVersion
; logic-sig-version :: monad positive-integer?
(record LogicSigVersion (monad logic-sig-version))

(define (logic-sig-version lsv)
  (match-define (LogicSigVersion logic-sig-version) lsv)
  logic-sig-version)

(provide LogicSigVersion
         logic-sig-version)
