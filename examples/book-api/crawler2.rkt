#lang racket

(require net/url)

;; See http://stackoverflow.com/questions/8935696/how-to-design-a-crawl-bot/8935755#8935755
(define curl-core-options
  (string-append
   "--silent "
   "--show-error "
   "--location "
   "--connect-timeout 10 "
   "--max-time 30 "
   "--cookie-jar " (path->string (build-path 'same "tmp" "cookies")) " "
   "--keepalive-time 60 "
   "--user-agent 'my crawler' "
   "--globoff " ))

(define (curl/head url out-file)
  (system (format "curl ~a --head --output ~a --url \"~a\""
                   curl-core-options
                   (path->string out-file)
                   url)))

(define (curl/get url out-file)
  (system (format "curl ~a --output ~a --url \"~a\""
                  curl-core-options
                  (path->string out-file)
                  url)))

 (curl/get "fititnt.org" (string->path "fititnt.txt"))