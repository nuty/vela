#lang scribble/manual
@title[#:style '(toc)]{Middleware}



@larger{
  Use Middleware 

}

Vela provided middleware whth two points: on-request and on-response:

you should add the middleware to url function or url-group function.

@codeblock|{

#lang racket
(require
  gregor
  vela
  web-server/http/request-structs)

(define (index req)
  (jsonify "hello!"))

(define (login-required req)
  (jsonify "user not login"))

(define (print-current-time req)
  (displayln (now)))

(define (say-hi req resp)
  (jsonify "hi"))

(define api-v1 
  (url-group "/cc" #:on-request (list login-required) #:on-response (list say-hi)))

(define routers
  (urls
    (url "/" index  #:on-request (list print-current-time) "index")

    (api-v1
      (url "/index" index)
      (url "/index1" index #:on-request (list login-required) #:on-response (list say-hi) "index1"))))

(app-run
  routers
  #:port 8000)
}|

