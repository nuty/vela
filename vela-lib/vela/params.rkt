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
  (define (parse-args req)
    (define hashs (parse-req req))
    (define args-hash (car hashs))
    (define json-hash (cdr hashs))
    (define result-hash (make-hash))
    (for ([key (hash-keys arguments-hash)])
      (let* 
        ([field (hash-ref arguments-hash key)]
         [name (hash-ref field 'name #f)]
         [location (hash-ref field 'location #f)]
         [type (hash-ref field 'type #f)]
         [require (hash-ref field 'require #f)]
         [default (hash-ref field 'default #f)]
         [filter (hash-ref field 'filter (void))])

          (void)  
        )
    )
    result-hash)
  parse-args)




(provide 
  argument
  arguments)