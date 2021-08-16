#lang racket/base
(require racket/match
         "monad.rkt"
         "read-byte.rkt")

(struct VM (MonadPlus ReadByte))

; execute : VM m s => byte → m ()
(define (execute vm)
  (match-define (VM (MonadPlus (Monad unit >>= >>) fail each) rb) vm)
  (match-lambda))

(provide VM
         execute)
