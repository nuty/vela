#lang racket
(require
  vela
  "handlers.rkt")



(define api-v1 (url-group "/api/v1"))

(define routers 
  (urls
    (url "/" api-handler "index")

    (api-v1
      (url "/books" book-handler "book-list/post")
      (url "/book/:book-id" book-handler "book-put/get/delete"))))


(displayln routers)


(app-run
  routers 
  #:port 7000)