#lang racket

(require
;   vela/config
  "../../../vela-lib/vela/main.rkt")


(define config
  (let ([env 'dev])
    (cond
      [(eq? env 'dev)
        (load-config "config/dev.yaml")]
      [(eq? env 'test)
        (load-config "config/test.yaml")]
      [(eq? env 'prod)
        (load-config "config/pord.yaml")]
      [else (displayln "errors")])))


(provide config)