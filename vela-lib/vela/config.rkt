#lang racket
(require 
  yaml
  json)


(define load-config
  (lambda (file-path)
    (define ip (open-input-file file-path))
    (yaml->hash ip)))


(define (hash->hasheq hsh)
  (cond [(list? hsh) (map hash->hasheq hsh)]
        [(not (or (hash? hsh) (list? hsh))) hsh]
        [else
         (let ([keys (hash-keys hsh)])
           (foldl (λ (key result)
                    (hash-set result (string->symbol key)
                      (hash->hasheq (hash-ref hsh key))))
                  (make-immutable-hasheq) keys))]))


(define (yaml->hash in-port)
  (let* ([contents (port->string in-port)]
          [yaml (string->yaml contents)]
          [conf-hash (hash->hasheq yaml)])
    conf-hash))


(define (set-config! config key value)
  (hash-set! config key value))


(provide
  load-config
  set-config!)