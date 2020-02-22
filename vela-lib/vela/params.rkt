#lang racket
(require
  json
  racket/struct
  net/url
  web-server/http/bindings
  web-server/http/request-structs
  "context.rkt")


(define (argument
    name
    #:type        type
    #:location    [location 'args]
    #:default     [default '()]
    #:require     [require #f]
    #:help        [help ""]
    #:filter      [filter (void)])
  (list name type location default require help filter))


(define (make-arguments-hash fields)
  (define argument-hash (make-hash))
  (for/list ([field (in-list fields)])
    (let*
      ([name (first field)]
        [type (second field)]
        [location (third field)]
        [default (fourth field)]
        [require (fifth field)]
        [help (sixth field)] 
        [filter (seventh field)])
      (cond 
        [(equal? filter (void)) 
         (hash-set! argument-hash name 
          (hash
            'name name
            'type type
            'location location
            'require require
            'help help))]
          [else
            (hash-set! argument-hash name 
              (hash 
                'name name
                'type type
                'location location
                'require require
                'help help
                'filter filter))])))
  argument-hash)


(define (parse-req req)
  (define json-hash (make-hasheq))
  (define args-hash (make-hasheq))
  (define maybe-json-data  (request-post-data/raw req))
  (define args-data (request-bindings req))
  (define content-type 
    (cdr (car 
      (filter (λ (header) (eq? (car header) 'content-type)) (request-headers req)))))

  (for ([arg args-data])
    (let
      ([key (car arg)]
       [value (cdr arg)])
       (hash-set! args-hash key value)))

  (if (and (not (empty? maybe-json-data))
           (equal? content-type "application/json"))
      (set! json-hash  (bytes->jsexpr maybe-json-data))
    (void))
  (cons args-hash json-hash))


(define (arguments . fields)
  (define arguments-hash (make-arguments-hash fields))
  (define (parse-args req)
    (define hashs (parse-req req))
    (define args-hash (car hashs))
    (define json-hash (cdr hashs))
    (define result-hash (make-hash))
    (for ([key (hash-keys arguments-hash)])
      (let* 
        ([field (hash-ref arguments-hash key)]
         [name (hash-ref field 'name)]
         [location (hash-ref field 'location 'args)])
        (cond 
          [(equal? location 'args)
            (extract-args name field args-hash result-hash)]
          [(equal? location 'form)
            (extract-args name field args-hash result-hash)]
          [else
            (extract-args name field json-hash result-hash)])))
    result-hash)
  parse-args)


(define (extract-args name field hash-args result-hash)
  (define type (hash-ref field 'type))
  (define value (hash-ref hash-args (string->symbol name) '()))
  (let ([type-case (case-type name value type (hash-ref field 'location 'args))]
        [field-filter (hash-ref field 'filter '())]
        [require (hash-ref field 'require)])
    (cond
      [(not (empty? value))
        (cond
          [(boolean? type-case)
            (if (empty? field-filter)
              (hash-set! result-hash name value)
            (hash-set! result-hash name (field-filter value)))]
          [else
            (hash-set! result-hash 'type-errors 
              (append
                (hash-ref result-hash 'type-errors (list))
                (list type-case)))])]
      [else
        (if (eq? require #t)
          (hash-set! result-hash 'required-errors
            (append
              (hash-ref result-hash 'required-errors (list))
              (list (format "argument '~a' is required" name))))
        (void))
      ]))
  result-hash)


(define (case-type name value type location)
  (cond
    [(eq? type 'number)

      (cond [(not (eq? value '()))
            (if (or (number? value) (number? (string->number value)))
              #t
            (format "argument '~a' is must type number" name))]
            [else (void)])
      ]
    [(eq? type 'str) (if (string? value) #t (format "argument '~a' is must type string" name))]
    [(eq? type 'list) (if (list? value) #t (format "argument '~a' is must type list" name))]
    [(eq? type 'hash) (if (hash? value) #t (format "argument '~a' is must type hash" name))]
    [else #f]))


(provide 
  argument
  arguments)