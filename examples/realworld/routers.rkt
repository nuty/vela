#lang racket
(require
  vela
  "./handlers/main.rkt")

(define routers 
  (urls
    (url "/" index "index")))

(provide routers)