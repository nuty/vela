#lang racket
(require 
  "../../vela-lib/vela/main.rkt"
  web-server/http/request-structs)


(define (index req)
  (jsonify "hello!"))

(define (on-req req)
  (jsonify "no token"))

(define (on-req1 req)
  (void))

(define (on-response req resp)
  (jsonify "hi"))

(define routers
  (urls
    (url "/" index  #:on-request (list on-req on-req1))
    (url "/11" index #:on-request (list on-req))
    (url "/22" index  "index")
    (url "/a" index #:on-request (list on-req on-req1) #:on-response (list on-response))
    (url "/aa" index #:on-response (list on-response))))


(app-run
  routers
  #:port 8000)