#lang racket/base
(require "record.rkt")

(record Monad (unit >>= >>))

(record MonadPlus (Monad mzero mplus))

(provide Monad
         MonadPlus)
