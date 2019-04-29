#lang racket
(require 
  vela)

(define hello-handler
  (class handler%

    (define/public (get [id null])
      (define request-context (get-field request this))
      (jsonify (hash 'code 200 'msg "handle get" )))

    (define/public (post)
      (jsonify (hash 'code 200 'msg "handle post" )))

    (define/public (put id)
      (displayln id)
      (jsonify (hash 'code 200 'msg "handle put" )))

    (define/public (delete id)
      (displayln id)
      (jsonify (hash 'code 200 'msg "handle delete" )))

    (super-new)))


(define routers
  (urls
    (url "/hello" hello-handler "hello-list/post")
    (url "/hello/:id" hello-handler "hello-put/delete/get-one")))

(app-run routers #:port 8000)

