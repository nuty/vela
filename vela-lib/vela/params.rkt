#lang racket
(require
  json
  racket/struct
  net/url
  web-server/http/bindings
  web-server/http/request-structs)


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
  
  (define json-hash (make-hash))
  (define args-hash (make-hash))
  (displayln "###################################")
  (displayln (request-post-data/raw req))
  (displayln "###################################")
  (define json-data (bytes->jsexpr (request-post-data/raw req)))
  (define args-data (request-bindings req))
  (displayln "###################################")

  (for ([arg args-data])
    (let
      ([key (car arg)]
       [value (cdr arg)])
       (hash-set! args-hash key value)))
  
  ; (displayln args-hash)
  (displayln (request-post-data/raw req))
  (displayln "###################################")

  )


(define (arguments . fields)
  (define arguments-hash (make-arguments-hash fields))
  (define queries-hash (make-hash))
  (define data-hash (make-hash))

  (define (parse-args req)
    (define result-hash (make-hash))
    (parse-req req)
    (for ([key (hash-keys arguments-hash)])
  ;   (displayln req)
      
      (void))
    
    result-hash)
  parse-args)




(provide 
  argument
  arguments)