#lang racket/base
(require
  web-server/http/request-structs)

(define urls-hash (make-hash))

(define default-headers
  (list
    (make-header #"Access-Control-Allow-Methods" #"PUT, POST, GET, DELETE, PATCH, OPTIONS")))

(define MIME-TYPE-HASH
  (hash
    "jpg" #"image/jpg; charset=utf-8"
    "jpeg" #"image/jpeg; charset=utf-8"
    "png" #"image/png; charset=utf-8"
    "gif" #"image/gif; charset=utf-8"
    "js" #"application/x-javascript; charset=utf-8"
    "xml" #"application/xml; charset=utf-8"
    "json" #"application/json; charset=utf-8"
    "css" #"text/css; charset=utf-8"
    "doc" #"application/msword; charset=utf-8"
    "docx" #"application/msword; charset=utf-8"
    "xls" #"application/excel; charset=utf-8"
    "pdf" #"image/pdf; charset=utf-8"
    "mp4" #"video/mpeg4; charset=utf-8"
    "mp3" #"audio/mp3; charset=utf-8"))

(provide
  default-headers
  urls-hash
  MIME-TYPE-HASH)