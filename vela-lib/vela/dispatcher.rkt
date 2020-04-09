#lang racket
(require
  web-server/http/request-structs
  web-server/servlet/servlet-structs
  (only-in web-server/servlet url->string)
  "context.rkt")


(define (request->route-key req routers static-path static-url)
  (let*
    ([req-full-path (url->string (request-uri req))]
     [url-prefix (if (empty? (string-split req-full-path "/"))
                    "/"
                  (first (string-split req-full-path "/")))])
    (cond
      [(equal? url-prefix static-url) (list url-prefix req-full-path)]
      [else
        (filter 
          (λ (key)
            (not (boolean? (regexp-match key req-full-path))))
              (hash-keys routers))])))


(define (static-file-handler req static-path static-url)
  (let*
    ([req-full-path (url->string (request-uri req))]
     [file-path (rest (string-split req-full-path static-url))]
     [file-type (last (string-split req-full-path "."))]
     [full-file-path (build-path static-path (substring (car file-path) 1))])
      (cond
        [(file-exists? full-file-path)
          (response-file full-file-path file-type)]
        [else
          (not-found req)])))


(define (route->handler req routers keys static-path static-url)
  (cond
    [(null? keys) (not-found req)]
    [else
      (let ([key (car keys)])
        (cond
          [(equal? key static-url) (static-file-handler req static-path static-url)] ;is static file url not handler url
          [else
            (handler->method req (hash-ref routers key) key)]))]))


(define (call/middlewares on-request on-response req handler args)
  (call/cc
    (lambda (exit)
      (let iter ((rest lst))
        (cond
          ((null? rest) 1)
          ((zero? (car rest)) (exit 0))
          (else (* (car rest) (iter (cdr rest))))
        )
      )
    )
  )
)


(define (handler->method req handler-hash key)
  (let 
    ([handler (hash-ref handler-hash 'handler)]
      [on-request (hash-ref handler-hash 'on-request)]
      [on-response (hash-ref handler-hash 'on-response)]
      [args (rest (regexp-match key (url->string (request-uri req))))])
    (cond 
      [(empty? on-request) (case-handler req handler args on-response)]
      [else (call/request-middlewares on-request on-response req handler args)])))


(define (call/request-middlewares on-request on-response req handler args)
  (let* 
    ([middlewares-rets (map (lambda (middleware) (middleware req)) (remove-duplicates on-request))]
      [mid-rets (filter (lambda (ret) (eq? ret #t)) 
                    (map (lambda (ret) (can-be-response? ret)) middlewares-rets))])
    (cond
      [(list? (member #t mid-rets)) (first middlewares-rets)]
      [(boolean? (member #t mid-rets)) (case-handler req handler args on-response)]
      [else (case-handler req handler args on-response)])))


(define (call/response-middlewares on-response req resp)
  (cond
    [(empty? on-response) resp]
    [else
      (let* 
        ([middlewares-rets (map (lambda (middleware) (middleware req resp)) (remove-duplicates on-response))]
          [mid-rets (filter (lambda (ret) (eq? ret #t)) 
                        (map (lambda (ret) (can-be-response? ret)) middlewares-rets))])
        (cond
          [(list? (member #t mid-rets)) (first middlewares-rets)]
          [(boolean? (member #t mid-rets)) resp]
          [else resp]))]))


(define (case-handler req handler args on-response)
  (cond
    [(procedure? handler) 
      (call/response-middlewares on-response req (if (empty? args) (handler req) (handler req args)))]
    [else
      (let ([handler-object (make-object handler req)])
        (begin
          (case (request-method req)
            [(#"GET") 
              (call/response-middlewares on-response req (send/apply handler-object get args))]
            [(#"POST")
              (call/response-middlewares on-response req (send/apply handler-object post args))]
            [(#"PUT") 
              (call/response-middlewares on-response req (send/apply handler-object put args))]
            [(#"PATCH") 
              (call/response-middlewares on-response req (send/apply handler-object patch args))]
            [(#"DELETE") 
              (call/response-middlewares on-response req (send/apply handler-object delete args))])))]))


(define (dispatcher req routers static-path static-url)
  (let
    ([keys (request->route-key req routers static-path static-url)])
      (route->handler req routers keys static-path static-url)))


(provide
  dispatcher)
