#lang racket
(require
  web-server/servlet-env
  web-server/http/request-structs
  "dispatcher.rkt")

(define app-hash (make-hash))

; (define vela-app
;   (lambda 
;     name
;     #:start-point [start-point "/"]
;     #:routers
;     #:config
;     #:on-request [on-request (list)]
;     #:on-response [on-response (list)]
;     #:plugins (list))
;   (begin
;     (hash-set! app-hash 'start-point start-point)
;     (hash-set! app-hash 'routers routers)
;     (hash-set! app-hash 'config config)
;     (hash-set! app-hash 'on-request on-request)
;     (hash-set! app-hash 'plugins plugins)
;     (hash-set! app-hash 'on-response on-response))
;   app-hash)


; (hash-set! 
;   urls-hash 
;   prefix
;   (hash
;     'handler handler
;     'args args
;     'on-request on-request
;     'on-response on-response
;     'endpoint endpoint))


(define handler%
  (class object%
    (super-new)
    (init-field request)

    (define/public (ctx)
      (get-field request this))))

; (define (vela-run app)
;   (void))

(define (app-run routers
  #:port    [host/port 8000]
  #:listen-ip    [listen-ip "127.0.0.1"]
  #:static-path  [static-path #f]
  #:static-url  [static-url #f]
  #:log-file [log-file #f])

  (serve/servlet
    (λ (req)
      (dispatcher req routers static-path static-url))
    #:launch-browser? #f
    #:servlet-path "/"
    #:port host/port
    #:listen-ip listen-ip
    #:log-file log-file
    #:servlet-regexp #rx""))

(provide
  handler%
  app-run)