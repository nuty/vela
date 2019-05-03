#lang racket
(require 
  vela
  "data.rkt")

(define api-handler 
  (class handler%

    (define/public (get)
      (jsonify (hash 'code 200 'msg "Hello API" )))

    (super-new)))

(define book-handler
  (class handler%

    (define/public (get [book-id null])
      (cond
        [(null? book-id) (jsonify (hash 'code 200 'data books))]
        [else
          (let*
            ([book-id-string (string->number book-id)]
             [book (filter (λ (book) (eq? book-id-string (hash-ref book 'id))) books)])
            (jsonify (hash 'code 200 'data book)))]))

    (define/public (post)
       (jsonify (hash 'code 200 'msg "handle post")))

    (define/public (put book-id)
       (jsonify (hash 'code 200 'msg "handle put")))

    (define/public (delete book-id)
       (jsonify (hash 'code 200 'msg "handle delete")))

    (super-new)))



(provide 
    api-handler
    book-handler)