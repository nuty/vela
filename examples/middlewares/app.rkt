#lang racket/base
(require
  "../../vela-lib/vela/main.rkt"
  web-server/http/request-structs)

(define (index-handler req)
  (jsonify "hello!"))

(define (login-required req)
  (jsonify "user not login"))

(define (print-current-time req)
  (displayln (current-seconds)))

(define (say-hi req resp)
  (jsonify "hi"))

(define api-v1 
  (url-group "/cc" #:on-request (list login-required)))


(define routers
  (urls
    (url "/" index-handler  #:on-request (list print-current-time) "index")
    (api-v1
      (url "/index" index-handler)
      (url "/index1" index-handler #:on-response (list say-hi) "index1"))))


(displayln routers)

(app-run routers #:port 7000)