#lang racket
(require
  vela)

(define index 
  (lambda (req)
    (jsonify "helloworld")))

(provide
  index)