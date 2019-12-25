#lang racket
(require
  web-server/templates
  racket/struct
  web-server/http/request-structs
  "../../vela-lib/vela/main.rkt")


(define hello-argumets
  (arguments
    (argument "name" #:type 'str #:location 'json #:default "rosso")
    (argument "age" #:type 'int #:location 'json)
    (argument "address" #:type 'str #:location 'json #:filter (lambda (x) (string-append "china" x)))
    (argument "id" #:type 'int #:location 'args)
    (argument "page" #:type 'int #:location 'args)
    (argument "house" #:type 'str #:location 'args)))



(define hello-handler%
  (class handler%
    (define/public (post id)
      (define ctx (get-field request this))
      (define args (hello-argumets ctx))
      ; (displayln ctx)
      (displayln "**************************************")
      (displayln (request-bindings/raw ctx))
      ; (displayln (request-post-data/raw ctx))
      (displayln "**************************************")
      (jsonify (hash 'code 200 'msg "handle post" )))

    (define/public (put id)
      (jsonify (hash 'code 200 'msg "handle put" )))

    (super-new)))


(define routers
  (urls
    (url "/:id" hello-handler% "hello-list/post")))


(app-run routers #:port 8000)