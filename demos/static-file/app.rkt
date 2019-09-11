#lang racket
(require
  web-server/templates
  vela)

(define (index req)
  (render (include-template "index.html")))


(define routers
  (urls
    (url "/" index "index-page")))


(app-run 
  routers 
  #:port 7000
  #:static-path (build-path (current-directory) "static") ;your static files dir
  #:static-url "static")