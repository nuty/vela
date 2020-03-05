#lang racket
(require 
  vela
  "./config/main.rkt"
  "./routers.rkt")

(displayln config)

(app-run 
  routers
  #:port (hash-ref config 'port))