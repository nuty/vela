#lang scribble/manual
@title[#:style '(toc)]{Argument Parsing}

While Vela provides easy access to request data (i.e. querystring or POST form encoded data), it’s still a pain to validate form data. Vela has built-in support for request data validation.

@defproc[(argument [lst list?])
         (listof
          (not/c (one-of/c 'procedure)))]

@defproc[(arguments [lst list?])
         (listof
          (not/c (one-of/c 'procedure)))]


@codeblock|{
#lang racket
(require vela)

(define hello-argumets
  (arguments
    (argument "name" #:type 'str #:location 'json)
    (argument "address" #:type 'str #:location 'json #:filter (lambda (x) x))
    (argument "backs" #:type 'str #:location 'json #:filter (lambda (x) x))
    (argument "age" #:type 'number #:location 'json #:require #t) 
    (argument "id" #:type 'number #:location 'args #:require #t)
    (argument "page" #:type 'number #:location 'args)))
}|