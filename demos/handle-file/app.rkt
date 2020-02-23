#lang racket
(require 
  "../../main.rkt"
  web-server/http/request-structs)

(define headers 
  (list
    (make-header #"Content-type" #"application/pdf; charset=utf-8")
    (make-header #"Content-Disposition" #"attachment;filename=filename.pdf")))


(define (file-handler req)
  (define file-path (path->string (build-path "files/file.pdf")))
  (response/file
    file-path
    headers))


(define routers
  (urls
    (url "/" (lambda (req) (jsonify "hello!")))
    (url "/file" file-handler "file")))


(app-run
  routers
  #:port 8000)