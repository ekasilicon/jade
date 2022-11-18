#lang racket/base
(require "static/record.rkt")

(record execution-context (approval-program
                           clear-state-program
                           global-num-byte-slice
                           global-num-uint
                           local-num-byte-slice
                           local-num-uint
                           global-state))

(provide execution-context)
