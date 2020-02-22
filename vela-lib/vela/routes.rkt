#lang racket

(require "meta.rkt")

(define (path->keys path)
  (map (λ (match) (string->symbol (substring match 2)))
    (regexp-match* #rx"/:([^\\/]+)" path)))


(define (path->regx path)
  (string-append
    "^"
    (regexp-replace* #rx":[^\\/]+" path "([^/?]+)")
    "(?:$|\\?)"))


(define (url 
          path
          handler
          #:on-request [on-request (list)]
          #:on-response [on-response (list)]
          [endpoint ""])
  (list path handler on-request on-response endpoint))


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
      [on-request (third u)]
      [on-response (fourth u)]
      [endpoint (last u)])
    (cond
      [(hash-has-key? urls-hash prefix) (error "Duplicate url key found.")]
      [else 
        (hash-set! urls-hash prefix
          (hash
            'handler handler
            'args args
            'on-request on-request
            'on-response on-response
            'endpoint endpoint))])))


(define (url-group 
  prefix 
  #:on-request [on-request (list)]
  #:on-response [on-response (list)])
  (define (group . us)
    (map 
      (lambda (e)
        (url
          (string-append prefix (first e))
          (second e)
          #:on-request (append on-request (third e))
          #:on-response (append on-response (fourth e))
          ;#:on-request (remove-duplicates (append on-request (third e)))
          ;#:on-response (remove-duplicates (append on-response (fourth e)))
          (last e))) us))
  group)


(provide
  url
  urls
  url-group)