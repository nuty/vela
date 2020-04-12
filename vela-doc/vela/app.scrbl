#lang scribble/manual
@title[#:style '(toc)]{Run App}


Vela provide a simple wrapper of (serve/servlet) call (run-app).

@defproc[#:link-target? #f
        (run-app 
        [routers urls?]
        [listen-ip "127.0.0.1"]
        [static-path path?]
        [static-url  string?]
        [log-file #f]
        [quit? #f])
        (serve/servlet)]

Here is an simple demo app.
@codeblock|{
#lang racket
(require
  vela/app
  vela/routes
  vela/dispatcher
  web-server/servlet-env)

(define index
  (lambda (req)
    (jsonify (hash 'msg "hello world!" ))))

(define routers
  (urls
    (url "/" index "index")))
}|

You can use (app-run) run it such like this
@codeblock|{
 
 ......

(app-run routers #:port 8000)

}|

Or if you want to use racket web-server internal (serve/servlet).

@codeblock|{

......


(define my-app 
  (lambda (req) 
    (dispatcher req routers)))

(serve/servlet
  my-app
  #:launch-browser? #f
  #:servlet-path "/"
  #:port 8000
  #:listen-ip "0.0.0.0"
  #:servlet-regexp #rx"")

}|

