#lang racket
(require
  web-server/servlet-env
  web-server/http/request-structs
  "dispatcher.rkt")


(define handler%

  (class object%
    (super-new)
    (init-field request)

    (define/public (ctx)
      (get-field request this))))


(define (app-run routers
  #:port    [host/port 8000]
  #:listen-ip    [listen-ip "127.0.0.1"]
  #:static-path  [static-path #f]
  #:static-url  [static-url #f]
  #:log-file [log-file #f]
  #:quit? [quit #t])

  (serve/servlet
    (λ (req)
      (dispatcher req routers static-path static-url))
    #:launch-browser? #f
    #:servlet-path "/"
    #:port host/port
    #:listen-ip listen-ip
    #:log-file log-file
    #:servlet-regexp #rx""
    #:quit quit))

(provide
  handler%
  app-run)
