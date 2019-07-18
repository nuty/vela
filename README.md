Vela
========

Simple web framework to build restful app in Racket.

Features
------------
- Write handlers use Function Based Handler or Class Based Handler.
- Friendly way to define url routers.
- Json response maker.
- Entirely on the racket webserver lib.


Installation
------------

`raco pkg install vela`

Quick code
-----------

```racket
#lang racket
(require vela)


(define hello-handler
  (class handler%

    (define/public (get [id null])
      (displayln id)
      (jsonify (hash 'code 200 'msg "handle get" )))

    (define/public (post)
      (jsonify (hash 'code 200 'msg "handle post" )))

    (super-new)))


(define fun-handler
  (lambda (req [id null])
    (jsonify (hash 'code 200 'msg id ))))


(define api-v1 (url-group "/api/v1")) ;define a url group

(define routers
  (urls

    (url "/" fun-handler "handler with function")
    (url "/another" fun-handler "handler with function")

    (api-v1
      (url "/hellos" hello-handler "hello-list/post")
      (url "/hello/:id" hello-handler "hello-put/delete/get"))))


(app-run
  routers
  #:port 8000
  #:static-path (build-path (current-directory) "static") ;your static files dir
  #:static-url "static") ;your static url suffix

```


Demos
----------
Very simple apps build with Vela in the [demos folder](https://github.com/nuty/vela/tree/master/demos).


TODO
----

### v0.2

- 😃 Route Grouping like [gin](https://github.com/gin-gonic/gin)
- [ ] Request middleware to solve problems such as user authentication etc.
- [ ] Integration request arguments schema features.


License
-------
Licensed under the MIT License.
