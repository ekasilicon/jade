#lang racket/base

(define OK "\x1b[32mOK\033[m")
(define ALERT "\x1b[33mALERT\033[m")
(define CRITICAL "\x1b[31mCRITICAL\033[m")

(provide OK ALERT CRITICAL)
