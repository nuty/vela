#lang racket
(require
  web-server/servlet-env
  web-server/http/request-structs)


(define middleware%
  (class object%

    (abstract process-request)

    (super-new)))


(provide
  middleware%)
