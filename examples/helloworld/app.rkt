#lang racket
(require
  json
  racket/struct
  web-server/http/request-structs

  "../../vela-lib/vela/main.rkt")


(define hello-argumets
  (arguments
    (argument "name" #:type 'str #:location 'json)
    (argument "address" #:type 'hash #:location 'json #:filter (lambda (x) x))
    (argument "backs" #:type 'list #:location 'json #:filter (lambda (x) x))
    (argument "age" #:type 'number #:location 'json #:require #t) 
    (argument "id" #:type 'number #:location 'args #:require #t)
    (argument "page" #:type 'number #:location 'args)))


(define hello-argumets1
  (arguments
    (argument "name" #:type 'str #:location 'form)
    (argument "address" #:type 'str #:location 'form #:filter (lambda (x) x))
    (argument "backs" #:type 'str #:location 'form #:filter (lambda (x) x))
    (argument "age" #:type 'number #:location 'form #:require #t) 
    (argument "id" #:type 'number #:location 'args #:require #t)
    (argument "page" #:type 'number #:location 'args)))



(define hello-handler%
  (class handler%

    (define/public (get id)
      (define req (get-field request this))
      (define args (hello-argumets req))

      (jsonify (hash 'code 200 'msg "handle get" )))


    (define/public (post id)
      (define req (get-field request this))
      ; (define args (hello-argumets req))
      (define args (hello-argumets1 req))
      (displayln args)
      (jsonify (hash 'code 200 'msg "handle post" )))



    (super-new)))


(define routers
  (urls
    (url "/:id" hello-handler% "hello-list/post")))


(app-run routers #:port 8000)