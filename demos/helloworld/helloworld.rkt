#lang racket
(require
  web-server/templates
  vela)

(define (index req)
  (let ([name "Rosso"])
    (render (include-template "index.html"))))

(define fun
  (lambda (req [id null])
    (jsonify (hash 'code 200 'msg id ))))

(define  (fun2 req id)
  (jsonify (hash 'code 200 'msg "handle fun2" )))

(define hello-handler%
  (class handler%

    (define/public (get [id null])
      (define ctx (get-field request this))
      (jsonify (hash 'code 200 'msg "handle get" )))

    (define/public (post)
      (jsonify (hash 'code 200 'msg "handle post" )))

    (define/public (put id)
      (jsonify (hash 'code 200 'msg "handle put" )))

    (define/public (delete id)
      (jsonify (hash 'code 200 'msg "handle delete" )))

    (super-new)))


(define routers
  (urls
    (url "/" index "index-page")
    (url "/hello" hello-handler% "hello-list/post")
    (url "/hello/:id" hello-handler% "hello-put/delete/get-one")
    (url "/fun" fun "fun")
    (url "/fun/:id" fun "fun-with-id")
    (url "/fun2/:id" fun2 "fun2-with-id")))


(app-run routers #:port 8000)