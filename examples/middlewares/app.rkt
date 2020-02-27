#lang racket
(require
  yaml
  vela
  web-server/http/request-structs)

(define file-yaml
  (file->yaml* "./config.yaml"))


(displayln (system-type 'vm))