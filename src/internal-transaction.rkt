#lang racket/base
(require "record.rkt")

(record InternalTransaction (begin next field submit access array))

(provide InternalTransaction)
