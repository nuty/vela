#lang racket
(require
  web-server/http/request-structs
 
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
          (response/file full-file-path file-type)]
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


(define (handler->method req handler-hash key)
  (cond
    [(eq? #"OPTIONS" (request-method req)) (options-response req)]
    [else
      (let
        ([handler (hash-ref handler-hash 'handler)]
          [args (rest (regexp-match key (url->string (request-uri req))))])
        (cond
          [(procedure? handler) (if (empty? args) (handler req) (handler req args))]
          [else
            (let ([handler-object (make-object handler req)])
              (case (request-method req)
                [(#"GET") (send/apply handler-object get args)]
                [(#"POST") (send/apply handler-object post args)]
                [(#"PUT") (send/apply handler-object put args)]
                [(#"PATCH") (send/apply handler-object patch args)]
                [(#"DELETE") (send/apply handler-object delete args)]))]))]))


(define (dispatcher req routers static-path static-url)
  (let
    ([keys (request->route-key req routers static-path static-url)])
      (route->handler req routers keys static-path static-url)))


(provide
  dispatcher)