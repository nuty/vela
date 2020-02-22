#lang racket
(require
  json
  web-server/http/response-structs
  "meta.rkt")


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

(define (not-found req)
  (response
    #:code 404
    #:body "404 not found!"
    #:message "Not Found"))


(define (options-response req [headers default-headers])
  (response #:headers headers))

(define (response-file file file-type)
  (let
    ([file-mime (hash-ref MIME-TYPE-HASH file-type #"text/html; charset=utf-8")])
    (response/output
      (λ (op)
        (let ([ip (open-input-file file)])
                (copy-port ip op)
              (close-input-port ip))) #:mime-type file-mime)))

(define (response/file file-path [headers default-headers])
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
(provide
  render
  jsonify
  options-response
  not-found
  response/file)
  response-file)






