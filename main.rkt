#lang racket
(require
  json
  web-server/servlet
  web-server/servlet-env
  web-server/http/bindings
  web-server/private/mime-types
  web-server/http/request-structs)

(define urls-hash (make-hash))

(define default-headers
  (list
    (make-header #"Access-Control-Allow-Origin" #"*")
    (make-header #"Access-Control-Allow-Credentials" #"true")
    (make-header #"Access-Control-Allow-Headers" #"*")
    (make-header #"Access-Control-Allow-Methods" #"PUT, POST, GET, DELETE, PATCH, OPTIONS")))


(define (response
  #:code    [code/kw 200]
  #:message [message/kw "OK"]
  #:seconds [seconds/kw (current-seconds)]
  #:mime    [mime/kw #f]
  #:headers [headers/kw empty]
  #:body    [body/kw empty])

  (define mime
    (cond [(string? mime/kw) (string->bytes/utf-8 mime/kw)]
          [(bytes? mime/kw) mime/kw]
          [else #f]))
  (define message
    (cond [(bytes? message/kw) message/kw]
          [(string? message/kw) (string->bytes/utf-8 message/kw)]
          [else #f]))
  (define body
    (cond [(string? body/kw) (list (string->bytes/utf-8 body/kw))]
          [(bytes? body/kw) (list body/kw)]
          [(list? body/kw) body/kw]
          (#t body/kw)))

  (response/full
    code/kw
    message
    seconds/kw
    mime
    headers/kw
    body))


(define (render
  ret
  #:code    [code 200]
  #:message [message "OK"]
  #:seconds [seconds (current-seconds)]
  #:mime    [mime TEXT/HTML-MIME-TYPE]
  #:headers [headers empty])
  (response
    #:code code
    #:message message
    #:seconds seconds
    #:mime mime
    #:headers headers
    #:body ret))


(define (jsonify args [headers default-headers])
  (let
    ([json-ret (with-output-to-string (λ () (write-json args)))])
    (response
      #:headers headers
      #:mime #"application/json"
      #:body json-ret)))


(define MIME-TYPE-HASH
  (hash
    "jpg" #"image/jpg; charset=utf-8"
    "jpeg" #"image/jpeg; charset=utf-8"
    "png" #"image/png; charset=utf-8"
    "gif" #"image/gif; charset=utf-8"
    "js" #"application/x-javascript; charset=utf-8"
    "xml" #"application/xml; charset=utf-8"
    "json" #"application/json; charset=utf-8"
    "css" #"text/css; charset=utf-8"
    "doc" #"application/msword; charset=utf-8"
    "docx" #"application/msword; charset=utf-8"
    "xls" #"application/excel; charset=utf-8"
    "pdf" #"image/pdf; charset=utf-8"
    "mp4" #"video/mpeg4; charset=utf-8"
    "mp3" #"audio/mp3; charset=utf-8"))


(define (not-found req)
  (response
    #:code 404
    #:body "404 not found!"
    #:message "Not Found"))


(define (options-response req [headers default-headers])
  (response #:headers headers))


(define handler%
  (class object%
    (super-new)
    (init-field request)

    (define/public (ctx)
      (get-field request this))))


(define (path->keys path)
  (map (λ (match) (string->symbol (substring match 2)))
    (regexp-match* #rx"/:([^\\/]+)" path)))


(define (path->regx path)
  (string-append
    "^"
    (regexp-replace* #rx":[^\\/]+" path "([^/?]+)")
    "(?:$|\\?)"))


(define (request->route-key req routers static-path static-url)
  (let*
    ([req-full-path (url->string (request-uri req))]
     [url-prefix (if (empty? (string-split req-full-path "/"))
                    "/"
                  (first (string-split req-full-path "/")))])
    (cond
      [(equal? url-prefix static-url) (list url-prefix req-full-path)]
      [else
        (filter (λ (key)
            (not (boolean? (regexp-match key req-full-path))))
              (hash-keys routers))])))


(define (static-file-handler req static-path static-url)
  (let*
    ([req-full-path (url->string (request-uri req))]
     [file-path (rest (string-split req-full-path static-url))]
     [file-type (last (string-split req-full-path "."))]
     [full-file-path (build-path static-path (substring (car file-path) 1))])
      (cond
        [(file-exists? full-file-path)
          (response-file full-file-path file-type)]
        [else
          (not-found req)])))


(define (route->handler req routers keys static-path static-url)
  (cond
    [(null? keys) (not-found req)]
    [else
      (let ([key (car keys)])
        (cond
          [(equal? key static-url) (static-file-handler req static-path static-url)] ;is static file url not handler url
          [else
            (handler->method req (hash-ref routers key) key)]))]))


(define (handler->method req handler-hash key)
  (cond
    [(eq? #"OPTIONS" (request-method req)) (options-response req)]
    [else
      (let
        ([handler (hash-ref handler-hash 'handler)]
          [args (rest (regexp-match key (url->string (request-uri req))))])
        (cond
          [(procedure? handler) (if (empty? args) (handler req) (handler req args))]
          [else
            (let ([handler-object (make-object handler req)])
              (case (request-method req)
                [(#"GET") (send/apply handler-object get args)]
                [(#"POST") (send/apply handler-object post args)]
                [(#"PUT") (send/apply handler-object put args)]
                [(#"PATCH") (send/apply handler-object patch args)]
                [(#"DELETE") (send/apply handler-object delete args)]))]))]))


(define (dispatcher req routers static-path static-url)
  (let
    ([keys (request->route-key req routers static-path static-url)])
      (route->handler req routers keys static-path static-url)))


(define (response-file file file-type)
  (let
    ([file-mime (hash-ref MIME-TYPE-HASH file-type #"text/html; charset=utf-8")])
    (response/output
      (λ (op)
        (let ([ip (open-input-file file)])
                (copy-port ip op)
              (close-input-port ip))) #:mime-type file-mime)))


(define (response/file file-path headers)
  (let*
    ([file-type (last (string-split file-path "."))]
      [file-mime (hash-ref MIME-TYPE-HASH file-type #"text/html; charset=utf-8")])
      (response/output
        (λ (op)
          (let ([ip (open-input-file file-path)])
              (copy-port ip op)
            (close-input-port ip)))
        #:mime-type file-mime
        #:headers headers)))


(define (urls . us)
  (for ([u us])
    (cond 
      [(string? (first u)) (make-url-hash u urls-hash)]
      [else 
        (for ([deep-u u]) 
          (make-url-hash deep-u urls-hash))]))
  urls-hash)


(define (make-url-hash u urls-hash)
  (let* 
    ([path (car u)]
      [prefix (path->regx path)]
      [handler (second u)]
      [args (path->keys path)]
      [endpoint (last u)])
    (cond
      [(hash-has-key? urls-hash prefix) (error "Duplicate url key found.")]
      [else 
        (hash-set! urls-hash prefix
          (hash
            'handler handler 
            'args args
            'endpoint endpoint))])))


(define (url-group prefix)
  (define (group . us)
    (map 
      (lambda (e) 
        (url
          (string-append prefix (first e))
          (second e)
          (last e))) us)) group)


(define (url path handler endpoint)
  (list path handler endpoint))


(define (app-run routers
  #:port    [host/port 8000]
  #:listen-ip    [listen-ip "127.0.0.1"]
  #:static-path  [static-path #f]
  #:static-url  [static-url #f]
  #:log-file [log-file #f]
  #:server-root-path [server-root-path (current-directory)]
  #:extra-files-paths [extra-files-paths (list (current-directory))]
  #:servlets-root [servlets-root (current-directory)])

  (serve/servlet
    (λ (req)
      (dispatcher req routers static-path static-url))
    #:launch-browser? #f
    #:servlet-path "/"
    #:port host/port
    #:listen-ip listen-ip
    #:log-file log-file
    #:servlet-regexp #rx""
    #:server-root-path server-root-path
    #:extra-files-paths extra-files-paths
    #:servlets-root servlets-root))


(provide
  handler%
  urls
  url
  app-run
  not-found
  url-group
  render
  response
  response/file
  jsonify)