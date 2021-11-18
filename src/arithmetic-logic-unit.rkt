#lang racket/base
(require "record.rkt")

(record ArithmeticLogicUnit (+ - / * % & \| ^ ~ < ==))

(provide ArithmeticLogicUnit)
