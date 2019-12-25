#lang racket


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


(define (arguments . fields)
  (define arguments-hash (make-arguments-hash fields))
  (define (parse-args req)
    (define result-hash (make-hash))
    (for ([key (hash-keys arguments-hash)])
      (void))
      result-hash)
  parse-args)




(provide 
  argument
  arguments)