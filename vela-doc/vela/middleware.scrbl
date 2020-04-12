#lang scribble/manual
@title[#:style '(toc)]{Middleware}



@larger{
  Use Middleware 
}

Vela provided middleware by two slots: on-request and on-response:

you should add the middleware to url function or url-group function.



@larger{
  Request middleware
}


Request middleware function need a request argument. such like this.

@codeblock|{
  (define (request-middleware req)
    ...)
}|


@larger{
  Response middleware
}


Response middleware function need two request arguments, request and response. such like this.

@codeblock|{
  (define (response-middleware req resp)
    ...)
}|


Whether it is request middleware or response middleware. When it returns a can-be-response?, 
the entire request will be interrupted and the responsed by the middleware will be returned.

This is an example to check user auth. if user is logged-in then cuntinue to next or 
if user not logged-in return some message.


@codeblock|{
  (define (login-required req)
    (if (is-logged-in req)
      (void)
    (jsonify "401")))
}|


@codeblock|{

#lang racket
(require
  vela
  web-server/http/request-structs)

;; handler
(define (index req)
  (jsonify "hello!"))

;; middlewares
(define (request-middleware req)
  (jsonify "hi!"))

(define (response-middleware req resp)
  (jsonify "bye!"))

;; url groups
(define req-urls
  (url-group "/req" #:on-request (list request-middleware)))

(define resp-urls
  (url-group "/resp" #:on-response (list response-middleware)))

(define req-and-resp-urls
  (url-group "/both" #:on-request (list request-middleware) #:on-response (list response-middleware)))


;; routers
(define middles-test-routers
  (urls
    (url "/req" index  #:on-request (list request-middleware) "request-middleware")
    (url "/resp" index  #:on-response (list response-middleware) "response-middleware")

    (req-urls
      (url "/test" index "test-req"))

    (resp-urls
      (url "/test" index "test-resp"))

    (req-and-resp-urls
      (url "/test" index "test-req-and-resp"))))


(app-run
  middles-test-routers
  #:port 8000)
}|

