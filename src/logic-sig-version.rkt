#lang racket/base
(require "record.rkt"
         "monad.rkt")

; class Monad monad => LogicSigVersion
; logic-sig-version :: monad positive-integer?
(record LogicSigVersion (monad logic-sig-version))

(provide LogicSigVersion)
