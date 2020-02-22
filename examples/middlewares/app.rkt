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
    (url "/a" index #:on-request (list print-current-time) #:on-response (list say-hi))
    (url "/aa" index #:on-response (list say-hi))
    (api-v1
      (url "/cc" index)
      (url "/dd" index #:on-response (list say-hi))
      (url "/ee" index #:on-request (list login-required))
      (url "/ff" index #:on-request (list login-required) #:on-response (list say-hi))
      (url "/hh" index #:on-request (list login-required) #:on-response (list say-hi) "index1"))))

(app-run
  routers
  #:port 8000)