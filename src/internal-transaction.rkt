#lang racket/base
(require "static/record.rkt")

(record InternalTransaction (begin next field submit access array))

(provide InternalTransaction)
