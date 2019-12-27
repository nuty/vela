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
            'default default
            'require require
            'help help))]
          [else
            (hash-set! argument-hash name 
              (hash 
                'name name
                'type type
                'location location
                'default default
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
  (define result-hash (make-hash))
  (define (parse-args req)
    (define hashs (parse-req req))
    (define args-hash (car hashs))
    (define json-hash (cdr hashs))
    (hash-set! result-hash 'errors (list))

    (for ([key (hash-keys arguments-hash)])
      (let* 
        ([field (hash-ref arguments-hash key)]
         [name (hash-ref field 'name)]
         [location (hash-ref field 'location 'args)])
        (cond 
          [(equal? location 'args) (extract-args "args" name field args-hash result-hash)]
          [else (extract-args "json" name field json-hash result-hash)])))
    result-hash)
  parse-args)


(define (case-type name value type location)
  (cond
    [(eq? type 'int) 
      (if (eq? location 'args) 
        (if (number? (string->number value)) #t (string-append "arg" " " name " " "int type error")) 
          (if (number? value) #t (string-append "arg" " " name " " "int type error"))) ]
    [(eq? type 'str) (if (string? value) #t (string-append "arg" " " name " " "string type error"))]
    [(eq? type 'list) (if (list? value) #t (string-append "arg" " " name " " "list type error"))]
    [(eq? type 'hash) (if (hash? value) #t (string-append "arg" " " name  " "  "hash type error"))]
    [(eq? type 'symbol) (if (symbol? value) #t (string-append "arg" " " name " " "symbol type error"))]
    [else #f]))
  


(define (extract-args from name field hash-args result-hash)

  (define type (hash-ref field 'type))
  (define value (hash-ref hash-args (string->symbol name) '()))
  (cond 
    [
      (not (empty? value))
      (let ([type-case (case-type name value type (hash-ref field 'location 'args))])
        (if (boolean? type-case) 
          (displayln type-case)
       
        (hash-set! result-hash 'errors (append (hash-ref result-hash 'errors) (list type-case)))
        )
      )
  ]
    [else 
      (displayln "值为空")])
)




(provide 
  argument
  arguments)