#lang racket/base
(require
  web-server/http/request-structs
  "../../vela-lib/vela/main.rkt")

(define headers 
  (list
    (make-header #"Content-type" #"application/octet-stream; charset=utf-8")
    (make-header #"Content-Disposition" #"attachment;filename=filename.pdf")))


(define (file-handler req)
  (define file-path (path->string (build-path "./file.pdf")))
  (response/file
    file-path
    headers))

(define routers
  (urls
    (url "/" (lambda (req) (jsonify "hello!")) "index")
    (url "/file" file-handler "file")))

(app-run
  routers
  #:port 8000)