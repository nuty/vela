#lang racket
(require
  vela
  "handlers.rkt")

(define routers 
  (urls
    (url "/" api-handler "indx")
    (url "/books" book-handler "book-list/post")
    (url "/book/:book-id" book-handler "book-put/get/delete")))

(app-run
  routers 
  #:port 7000)