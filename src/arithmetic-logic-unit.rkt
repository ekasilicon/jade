#lang racket/base
(require "static/record.rkt")

(record ArithmeticLogicUnit (+ - / * % & \| ^ ~ < ==))

(provide ArithmeticLogicUnit)
