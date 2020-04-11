#lang racket
(require
  json
  racket/struct
  web-server/http/request-structs
  web-server/http/response-structs
  "../../vela-lib/vela/main.rkt")


(define hello-argumets
  (arguments
    (argument "name" #:type 'str #:location 'form)
    (argument "address" #:type 'str #:location 'form #:filter (lambda (x) x))
    (argument "age" #:type 'number #:location 'form #:require #t) 
    (argument "aaa" #:type 'str #:location 'args #:require #t)
    (argument "hello" #:type 'str #:location 'form)
    (argument "id" #:type 'number #:location 'args #:require #t)))

(define index
  (lambda (req id ad bd)
    (displayln id)
    (displayln ad)
    (displayln bd)
    (jsonify (hash 'msg "hello world!"))))

(define index1
  (lambda (req)
    (jsonify (hash 'msg "hello world!" ))))



(define hello-handler%
  (class handler%

    (define/public (get id)

      (jsonify (hash 'code 200 'msg id)))

    (super-new)))


(define api-v1 (url-group "/api/v1"))


(define routers
  (urls
    (url "/" index1 "index1")
    (url "/:id/:ad/:bd" index "index")
    (api-v1
      (url "/:id" hello-handler% "end-point"))))


(displayln routers)


(app-run routers #:port 8000)