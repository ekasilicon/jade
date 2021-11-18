#lang racket/base
(require racket/match
         (for-syntax racket/base
                     racket/match
                     syntax/parse))

(begin-for-syntax
  (define ((make-field-index flds) fld-stx)
    (let ([fld (syntax->datum fld-stx)])
          (cond
            [(for/first ([i (in-naturals)]
                         [fld₀ (in-list flds)]
                         #:when (eq? fld₀ fld))
               i)
             => values]
            [else
             (raise-syntax-error #f "unknown field" fld-stx)])))
  (define-syntax-class field-init
    #:attributes [field expression]
    (pattern fld:id
             #:attr field #'fld
             #:attr expression #'fld)
    (pattern [fld:id exp]
             #:attr field #'fld
             #:attr expression #'exp))
  (struct record-info (fields
                       type
                       constructor
                       predicate
                       accessor
                       mutator)
    #:property prop:procedure
    (λ (self stx)
      (match-let ([(record-info flds type cons ? ref set!) self])
        (define field-index (make-field-index flds))
        (with-syntax ([cons cons]
                      [ref ref])
          (syntax-parse stx
            [(_ init:field-init ...)
             (let ([xs (generate-temporaries #'(init ...))])
               (with-syntax ([(x ...) xs]
                             [(y ...) (let ([locs (for/fold ([locs (hasheqv)])
                                                            ([fld (in-list (syntax->list #'(init.field ...)))]
                                                             [x (in-list xs)])
                                                    (let ([i (field-index fld)])
                                                      (if (hash-has-key? locs i)
                                                        (raise-syntax-error #f "duplicate field" fld)
                                                        (hash-set locs i x))))])
                                        (for/list ([i (in-naturals)]
                                                   [fld (in-list flds)])
                                          (cond
                                            [(hash-ref locs i #f)
                                             => values]
                                            [else
                                             (raise-syntax-error #f (format "~a field not present" fld) stx)])))])
                 #'(let ([x init.expression] ...) (cons y ...))))]))))
    #:property prop:match-expander
    (λ (self stx)
      (match-let ([(record-info flds type cons ? ref set!) self])
        (define field-index* (make-field-index flds))
        (define-syntax-class field-pat
          #:attributes [field field-index pattern]
          (pattern fld:id
                   #:attr field #'fld
                   #:attr field-index (datum->syntax #'fld (field-index* #'fld))
                   #:attr pattern #'fld)
          (pattern [fld:id pat]
                   #:attr field #'fld
                   #:attr field-index (datum->syntax #'fld (field-index* #'fld))
                   #:attr pattern #'pat))
        (with-syntax ([pred ?]
                      [ref ref])
          (syntax-parse stx
            [(_ fp:field-pat ...)
             #'(? pred (and (app (λ (x) (ref x fp.field-index)) fp.pattern)
                            ...))])))))
  (provide record-info record-info? field-init))

(define-syntax record
  (syntax-parser
    [(_ name:id (field:id ...))
     (with-syntax ([num-fields (length (syntax->list #`(field ...)))])
       #'(begin
           (define-values (type cons ? ref set!)
             (make-struct-type 'name
                               #f
                               num-fields
                               0
                               #f
                               (list)
                               'prefab))
           (define-syntax name
             (record-info '(field ...) #'type #'cons #'? #'ref #'set!))))]))

(provide record)

(module+ test
  (require rackunit)
  
  (record big (foo bar baz))

  (let* ([z 1]
         [y (big [baz (begin (set! z 10) 1)]
                 [bar (begin (set! z 12) 2)]
                 [foo (begin (set! z 11) 3)])])
    (check-equal? z 11 "field reordering"))
  
  (check-equal? (match (let ([foo "this"]
                             [bar "that"]
                             [baz "what"])
                         (big baz foo [bar "not that"])) 
                  [(big baz foo bar [bar b])
                   (list baz foo bar b)])
                (list "what" "this" "not that" "not that"))

  (let ([x (big [foo 10]
                [bar 12]
                [baz 11])])
    (check-equal? (match x
                    [(big [foo z] [bar x])
                     (list z x)])
                  (list 10 12)))
  
  #;
  (match x
    [(test foo [bar x])
     (list foo x)])
  #;
  (match x
    [(test bar foo)
     (list foo bar)])

  #;(test foo)
  #;((test foo) x) 
  #;
  (match x
    [(test [foo _] [bar b])
     b]))

#|
#lang racket/base
(require racket/match
         (for-syntax racket/base
                     racket/match
                     syntax/parse
                     syntax/parse/class/struct-id
                     #;
                     racket/struct-info))

(define-match-expander record
  (syntax-parser
    [(_ id:struct-id [field:id value] ...)
     (with-syntax ([x #'(id.accessor-id ...)])
       #''x)
     #;
     (match (extract-struct-info (syntax-local-value #'struct))
       [(list struct-id name-id )])])
  (syntax-parser
    [(_ id:struct-id [field:id value] ...)
     (with-syntax ([accessors #'(id.accessor-id ...)]
                   [suppliers #'(field ...)])
       #''(accessors suppliers))
     #;
     (match (extract-struct-info (syntax-local-value #'struct))
       [(list struct-id name-id )])])
  )




(module+ main
  (struct foo (bar baz) #:transparent)

(record foo [baz 2] [bar 1])

#;
  (match (record foo [bar 1] [baz 2])
    [(record foo [bar x] [baz y])
     (list x y)]))
|#
