#lang racket/base
(require "record.rkt")

(record Monad (unit >>= >>))

(record Monad+ (monad mzero mplus))

(provide Monad
         Monad+)
