#lang racket/base
(require (only-in racket/match define-match-expander))

(struct exn:fail:jade exn:fail (tag) #:transparent)

(define (make-jade-error tag template . args)
  (raise (exn:fail:jade (apply format template args)
                        (current-continuation-marks)
                        tag)))

(define-match-expander jade-error
  (syntax-rules ()
    [(jade-error tag message)
     (exn:fail:jade message _ tag)])
  (syntax-rules ()
    [(jade-error tag template arg ...)
     (make-jade-error tag template arg ...)]))

(provide jade-error
         (rename-out [exn:fail:jade? jade-error?]))
