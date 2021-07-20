#lang racket/base
(require racket/match)

(struct environment (logic-sig-version approval-program) #:transparent)
(struct state (pc stack) #:transparent)

(struct result (values state) #:transparent)
(struct failure (message) #:transparent)

(define ((unit . xs) r st) (result xs st))
(define ((>>= m f) r st)
  (match (m r st)
    [(result xs st)
     ((apply f xs) r st)]
    [x x]))
(define (fail template . args)
  (failure (apply format template args)))

(define (unused bc)
  (fail "unrecognized instruction ~a" (number->string bc 16)))

(define (unimplemented bc)
  (fail "need to implement ~a" (number->string bc 16)))

(define (read-byte r st)
  (let ([bs (environment-approval-program r)])
    (result (list (bytes-ref bs (state-pc st)))
            (struct-copy state st [pc (+ (state-pc st) 1)]))))

(define execute-hash unimplemented)
(define execute-push unimplemented)
(define execute-app unimplemented)
(define execute-params unimplemented)
(define execute-extract unimplemented)
(define execute-stk unimplemented)
(define execute-ctl unimplemented)
(define execute-env unimplemented)
(define execute-arg unimplemented)
(define execute-bytec unimplemented)
(define execute-intc unimplemented)
(define execute-al unimplemented)
(define execute-aw unimplemented)
(define execute-call unimplemented)
(define execute-byteop unimplemented)

(define-match-expander range
  (syntax-rules ()
    [(_ lo hi)
     (? (λ (bc) (<= lo bc hi)))]))

(define (execute bc)
  (match bc
    [#x00 ; fail
     (fail "err instruction")]
    [(range #x01 #x04)
     (execute-hash bc)]
    [(range #x05 #x07)
     (unused bc)]
    [(range #x08 #x1c)
     (execute-al bc)]
    [(range #x1d #x1f)
     (execute-aw bc)]
    [(range #x20 #x25)
     (execute-intc bc)]
    [(range #x26 #x2b)
     (execute-bytec bc)]
    [(range #x2c #x30)
     (execute-arg bc)]
    [(range #x31 #x3d)
     ; this is significant
     ; and should probably be factored
     (execute-env bc)]
    [(range #x3e #x3f)
     (unused bc)]
    [(range #x40 #x44)
     (execute-ctl bc)]
    [(range #x45 #x47)
     (unused bc)]
    [(range #x48 #x4d)
     (execute-stk bc)]
    [(range #x4e #x4f)
     (unused bc)]
    [(range #x50 #x5b)
     ; this is significant
     (execute-extract bc)]
    [(range #x60 #x69)
     ; this is significant
     (execute-app bc)]
    [(range #x6a #x6f)
     (unused bc)]
    [(range #x70 #x72)
     (execute-params bc)]
    [#x78 ; min_balance
     (execute-app bc)]
    [(range #x79 #x7f)
     (unused bc)]
    [(range #x80 #x81)
     (execute-push bc)]
    [(range #x82 #x87)
     (unused bc)]
    [(range #x88 #x89)
     (execute-call bc)]
    [(range #x8a #x8f)
     (unused bc)]
    [(range #x90 #x94)
     (execute-al bc)]
    [#x95 ; expw
     (execute-aw bc)]
    [(range #x96 #x9f)
     (unused bc)]
    [(range #xa0 #xaf)
     (execute-byteop bc)]
    [(range #xb0 #xff)
     (unused bc)]))

(define → (>>= read-byte execute))

(define (teal-execute teal-bytes)
  (let ([r (environment (bytes-ref teal-bytes 0) teal-bytes)])
    (let loop ([st (state 1 (list))])
      (match (→ r st)))))

(module+ main
  (require racket/file)
  (match (current-command-line-arguments)
    [(vector filename)
     (teal-execute (file->bytes filename))]))
