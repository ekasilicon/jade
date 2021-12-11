#lang racket/base
(require racket/match
         "static/record.rkt"
         "monad.rkt")

; class Monad m => LogicSigVersion
; logic-sig-version :: m positive-integer?
(record LogicSigVersion Monad (logic-sig-version))

(define (logic-sig-version lsv)
  (match-define (LogicSigVersion logic-sig-version) lsv)
  logic-sig-version)

(provide LogicSigVersion
         logic-sig-version)
