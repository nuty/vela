#lang racket
(require
  json
  racket/struct
  web-server/http/request-structs

  "../../vela-lib/vela/main.rkt")


(define hello-argumets
  (arguments
    (argument "name" #:type 'str #:location 'json #:default "rosso")
    (argument "address" #:type 'str #:location 'json #:filter (lambda (x) (string-append "china" x)))
    (argument "house" #:type 'str #:location 'json)
    (argument "age" #:type 'int #:location 'json)
    (argument "id" #:type 'int #:location 'args)
    (argument "page" #:type 'int #:location 'args)))



(define hello-handler%
  (class handler%

    (define/public (get id)
      (define req (get-field request this))
      (define args (hello-argumets req))
      (jsonify (hash 'code 200 'msg "handle put" )))


    (define/public (post id)
      (define req (get-field request this))
      (define args (hello-argumets req))
      (jsonify (hash 'code 200 'msg "handle post" )))



    (super-new)))


(define routers
  (urls
    (url "/:id" hello-handler% "hello-list/post")))


(app-run routers #:port 8000)