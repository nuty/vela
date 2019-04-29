#lang racket

(require 
  json
  web-server/servlet
  web-server/servlet-env
  web-server/http/bindings
  web-server/http/request-structs)

(define default-headers (list
      (make-header #"Access-Control-Allow-Origin" #"*")
      (make-header #"Access-Control-Allow-Credentials" #"true")
      (make-header #"Access-Control-Allow-Headers" #"*")
      (make-header #"Access-Control-Allow-Headers" #"Authorization, Access-Control-Allow-Headers, X-Mx-ReqToken, Origin, Accept, X-Requested-With, Content-Type, Access-Control-Request-Method, Access-Control-Request-Headers")))

(define (response
  #:code    [code/kw 200]
  #:message [message/kw "OK"]
  #:seconds [seconds/kw (current-seconds)]
  #:mime    [mime/kw #f]
  #:headers [headers/kw empty]
  #:body    [body/kw empty])
  (define mime
    (cond [(string? mime/kw) (string->bytes/utf-8 mime/kw)]
          [(bytes? mime/kw) mime/kw]
          [else #f]))
  (define message
    (cond [(bytes? message/kw) message/kw]
          [(string? message/kw) (string->bytes/utf-8 message/kw)]
          [else #f]))
  (define body
    (cond [(string? body/kw) (list (string->bytes/utf-8 body/kw))]
          [(bytes? body/kw) (list body/kw)]
          [(list? body/kw) body/kw]
          (#t body/kw)))
  (response/full
    code/kw
    message
    seconds/kw
    mime
    headers/kw
    body))

(define (jsonify args)
  (define json-rv 
    (with-output-to-string
        (λ () (write-json args))))
  (response
    #:headers default-headers
    #:mime #"application/json"
    #:body json-rv))

(define handler%
  (class object%
    (super-new)
    (init-field request)
    
    (define/public (request-context)
      (get-field request this))))
 
(define (path->keys path)
  (map (λ (match) (string->symbol (substring match 2)))
    (regexp-match* #rx"/:([^\\/]+)" path)))

(define (path->regx path)
  (string-append
    "^"
    (regexp-replace* #rx":[^\\/]+" path "([^/?]+)")
    "(?:$|\\?)"))

(define (request->route-key req routers)
  (filter (λ (key)
          (not (boolean? (regexp-match key (url->string (request-uri req))))))
        (hash-keys routers)))

(define (not-found req)
  (response 
    #:code 200
    #:body "404 not found!"
    #:message "Not Found"))

(define (route->handler req routers keys)
  (cond
    [(null? keys) (not-found req)]
    [else
      (let ([key (car keys)])
        (handler->method req (hash-ref routers key) key))]))

(define (handler->method req handler-hash key)
  (let* 
    ([handler (hash-ref handler-hash 'handler)]
     [args (rest (regexp-match key (url->string (request-uri req))))]
     [handler-object (make-object handler req)])
    (case (request-method req)
      [(#"GET") (send/apply handler-object get args)]
      [(#"POST") (send/apply handler-object post args)]
      [(#"PUT") (send/apply handler-object put args)]
      [(#"PATCH") (send/apply handler-object patch args)]
      [(#"DELETE") (send/apply handler-object delete args)])))

(define (dispatcher req routers)
  (let
    ([keys (request->route-key req routers)])
    (route->handler req routers keys)))

(define (urls . us)
  (define urls-hash (make-hash))
  (for ([u us])
    (let* 
      ([path (car u)]
       [prefix (path->regx path)]
       [handler (second u)]
       [args (path->keys path)]
       [endpoint (last u)])
      (cond
        [(hash-has-key? urls-hash prefix) (error "Duplicate url key found.")]
        [else 
          (hash-set! urls-hash prefix
            (hash
              'handler handler 
              'args args
              'endpoint endpoint))]))) urls-hash)

(define (url path handler endpoint)
  (list path handler endpoint))

(define (app-run routers
  #:port    [host/port 8000]
  #:config   [app/config #f])
  (serve/servlet
    (λ (req)
        (dispatcher req routers))
    #:launch-browser? #f
    #:servlet-path "/"
    #:port host/port
    #:listen-ip #f
    #:servlet-regexp #rx""))

(provide
  handler%
  urls
  url
  app-run
  not-found
  jsonify)