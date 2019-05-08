#lang racket
(require
  racket/runtime-path
  web-server/templates
  web-server/servlet
  web-server/servlet-env
  web-server/dispatch
  (only-in "../../main.rkt"
    urls
    app-run)
  (rename-in "../../main.rkt"
    (url vurl)))


(define (index req)
  (let ([name "Rosso"])
    (render (include-template "templates/index.html"))))

(define routers
  (urls
    (vurl "/index" index "index-page")))


(app-run routers 
  #:port 8000 
  #:static-path (build-path (current-directory) "static")
  #:static-url "static"
)
