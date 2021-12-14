#lang racket/base
(require racket/match
         (for-syntax racket/base
                     racket/match
                     syntax/parse))

; add default field expressions in which field names are in scope?

(begin-for-syntax
  (define ((make-constructor-position info) fld-stx)
    (let ([fld (syntax->datum fld-stx)])
      (let loop ([info info])
        (cond
          [(for/first ([i (in-naturals)]
                       [fld₀ (in-list (record-info-fields info))]
                       #:when (eq? fld₀ fld))
             i)
           => (λ (i)
                (+ i
                   (let loop ([super-id (record-info-super-id info)])
                     (if super-id
                       (let ([super-info (syntax-local-value super-id)])
                         (+ (length (record-info-fields super-info))
                            (loop (record-info-super-id super-info))))
                       0))))]
          [else
           (let ([super-id (record-info-super-id info)])
             (if super-id
               (loop (syntax-local-value super-id))
               (raise-syntax-error #f "unknown field" fld-stx)))]))))
  (define-syntax-class field-init
    #:attributes [field expression]
    (pattern fld:id
             #:attr field #'fld
             #:attr expression #'fld)
    (pattern [fld:id exp]
             #:attr field #'fld
             #:attr expression #'exp))
  (struct record-info (super-id
                       fields
                       struct-type
                       constructor
                       predicate
                       accessor)
    #:property prop:procedure
    (λ (self stx)
      (define constructor-position (make-constructor-position self))
      (syntax-parse stx
          [(_ init:field-init ...)
           (let ([xs (generate-temporaries #'(init ...))])
             (with-syntax ([cons (record-info-constructor self)]
                           [(x ...) xs]
                           [(y ...) (let ([locs (for/fold ([locs (hasheqv)])
                                                          ([fld (in-list (syntax->list #'(init.field ...)))]
                                                           [x (in-list xs)])
                                                  (let ([i (constructor-position fld)])
                                                    (if (hash-has-key? locs i)
                                                      (raise-syntax-error #f "duplicate field" fld)
                                                      (hash-set locs i x))))])
                                      (for/list ([i (in-naturals)]
                                                 [fld (in-list (let loop ([info self])
                                                                 (append (let ([super-id (record-info-super-id info)])
                                                                           (if super-id
                                                                             (loop (syntax-local-value super-id))
                                                                             (list)))
                                                                         (record-info-fields info))))])
                                        
                                        (cond
                                          [(hash-ref locs i #f)
                                           => values]
                                          [else
                                           (raise-syntax-error #f (format "~a field not present" fld) stx)])))])
               #'(let ([x init.expression] ...) (cons y ...))))]))
    #:property prop:match-expander
    (λ (self stx)
      (define ((make-make-accessor info) fld-stx)
        (let ([fld (syntax->datum fld-stx)])
          (let loop ([info info])
            (cond
              [(for/first ([i (in-naturals 0)]
                           [fld₀ (in-list (record-info-fields info))]
                           #:when (eq? fld₀ fld))
                 i)
               => (λ (i)
                    (with-syntax ([ref (record-info-accessor info)]
                                  [i i])
                      #'(λ (x) (ref x i))))]
              [else
               (let ([super-id (record-info-super-id info)])
                 (if super-id
                   (loop (syntax-local-value super-id))
                   (raise-syntax-error #f "unknown field" fld-stx)))]))))
      (define make-accessor (make-make-accessor self))
      (define-syntax-class field-pat
        #:attributes [field accessor pattern]
        (pattern fld:id
                 #:attr field #'fld
                 #:attr accessor (make-accessor #'fld)
                 #:attr pattern #'fld)
        (pattern [fld:id pat]
                 #:attr field #'fld
                 #:attr accessor (make-accessor #'fld)
                 #:attr pattern #'pat))
      (syntax-parse stx
        [(_ fp:field-pat ...)
         (with-syntax ([pred (record-info-predicate self)])
           #'(? pred (and (app fp.accessor fp.pattern) ...)))])))
  (provide record-info record-info? field-init))

(define-syntax record
  (syntax-parser
    [(_ name:id (field:id ...))
     (with-syntax ([num-fields (length (syntax->list #'(field ...)))])
       #'(begin
           (define-values (type cons ? ref set!)
             (make-struct-type 'name
                               #f
                               num-fields
                               0
                               #f
                               (list)
                               'prefab
                               #f
                               (build-list num-fields values)))
           (define-syntax name
             (record-info #f '(field ...) #'type #'cons #'? #'ref))))]
    [(_ name:id super:id (field:id ...))
     (cond
       [(syntax-local-value #'super (λ () #f))
        => (λ (info)
             (if (record-info? info)
               (cond
                 [(for/first ([fld-stx (in-list (syntax->list #'(field ...)))]
                              #:when (let ([fld (syntax->datum fld-stx)])
                                       (let loop ([info info])
                                         (or (memq fld (record-info-fields info))
                                             (let ([super-id (record-info-super-id info)])
                                               (if super-id
                                                 (loop (syntax-local-value super-id))
                                                 #f))))))
                    fld-stx)
                  => (λ (fld-stx)
                       (raise-syntax-error #f "duplicate field" fld-stx))]
                 [else
                  (with-syntax ([super-type (record-info-struct-type (syntax-local-value #'super (λ () #f)))]
                                [num-fields (length (syntax->list #'(field ...)))]
                                [(type cons ? ref set!) (generate-temporaries #'(type cons ? ref set!))])
                    #'(begin
                        (define-values (type cons ? ref set!)
                          (make-struct-type 'name
                                            super-type
                                            num-fields
                                            0
                                            #f
                                            (list)
                                            'prefab
                                            #f
                                            (build-list num-fields values)))
                        (define-syntax name
                          (record-info #'super '(field ...) #'type #'cons #'? #'ref))))])
               (raise-syntax-error #f "not bound to a record identifier" #'super)))]
       [else
        (raise-syntax-error #f "not bound to a record identifier" #'super)])]))

(provide record)

#;
(let ([w 45])
  (record-copy Boo x
               #:update y f
               #:shadow z u
               #:shadow w))

#;
(define-syntax record-copy
  (syntax-parser
    [(_ name:id 
        )]))


(module+ test
  (require rackunit))

(module+ test
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
                  (list 10 12))))

(define-syntax record-predicate
  (syntax-parser
    [(_ recname:id)
     (match (syntax-local-value #'recname (λ () #f))
       [(record-info _ _ _ _ ? _)
        ?]
       [_
        (raise-syntax-error 'record-predicate "not a declared record" #'recname)])]))

(provide record-predicate)

(module+ test
  (check-equal? (let ([x (big [foo 1] [bar 2] [baz 3])])
                  ((record-predicate big) x))
                #t))

(module+ test
  (record A (a b c))
  (record D A (d))

  (D [d 'd] [a 'a] [b 'b] [c 'c])
  
  (A [a 10] [c 12] [b 45])

  (let ([a 12])
    (A a
       [c 10]
       [b 20]))

  (match (D [a 10]
            [b 12]
            [c 13]
            [d 14])
    [(A c [b 12])
     c])

  #;
  (record E A (a)))
