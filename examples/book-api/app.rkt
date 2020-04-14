#lang racket/base
(require
  "../../vela-lib/vela/main.rkt"
  "handlers.rkt")


(define api-v1 (url-group "/api/v1"))

(define routers 
  (urls
    (url "/" api-handler "index")

    (api-v1
      (url "/books" book-handler "book-list/post")
      (url "/book/:book-id" book-handler "book-put/get/delete"))))

(app-run
  routers 
  #:port 7000)