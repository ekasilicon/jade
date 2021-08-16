#lang racket/base

(struct Monad (unit >>= >>))

(struct MonadPlus (Monad mzero mplus))

(provide Monad
         MonadPlus)
