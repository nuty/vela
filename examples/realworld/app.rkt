#lang racket
(require 
  vela
  "./config/main.rkt"
  "./routers.rkt")

(app-run routers #:port 9000)