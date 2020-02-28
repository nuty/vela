#lang racket
(require 
  vela
  "./config/main.rkt"
  "./routers.rkt")


(define log (hash-ref config 'log))

(define f (string->symbol log))

(displayln (string? f))
(displayln (symbol? f))


(app-run routers #:port 9000)