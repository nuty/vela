Vela
========
Simple web framework to build restful app in Racket.

Features
------------
- Web handlers with Function or Class.
- Friendly way to define url routers.
- Pluggable middlewares when request and response.
- Request params check and collector.
- Entirely on the racket webserver lib.


Installation
------------

`raco pkg install vela`


Quickstart
------------

```racket
  #lang racket
  (require vela)

  (define index
    (lambda (req)
      (jsonify (hash 'msg "hello world!" ))))

  (define routers
    (urls
      (url "/" index "index")))

  (app-run routers #:port 8000)
```


Use Vela
-----------
```racket
#lang racket
(require vela)
```


Define Handler
-----------
There two ways to define a handler.

Use ```handler%```

```racket
(define hello-handler
  (class handler%

    (define/public (get [id null])
      (displayln id)
      (jsonify (hash 'code 200 'msg "handle get" )))

    (define/public (post)
      (jsonify (hash 'code 200 'msg "handle post" )))

    (define/public (put id)
      (jsonify (hash 'code 200 'msg "handle put" )))

    (define/public (delete id)
      (jsonify (hash 'code 200 'msg "handle delete" )))

    (super-new)))

```

Use simple function:

```racket
(define index-handler
  (lambda (req)
    (jsonify (hash 'code 200 'msg "hello api" ))))

```

  or

```racket
(define (index-handler req)
  (jsonify (hash 'code 200 'msg "hello api" )))

```

Response
-----------
Use ```jsonify ``` to return JSON content.

```racket
(jsonify (hash 'name "rosso" 'msg "hello world!"))
```

Use ```render ``` to render TEXT or HTML template.

plain text

```racket
(render "hello world!")
```


HTML template

```racket
(require web-server/templates)
  ...
(render (include-template "index.html"))
```


URL routes
-----------

Use ```urls``` and ```url``` function to define route.

```racket
(define routers
  (urls
    (url "/" index-handler "handler with function")))
```

Use ```url-group``` grouping routes.


```racket
(define api-v1 (url-group "/api/v1"))

(define routers
  (urls
	 ...
    (api-v1
      (url "/hellos" hello-handler "hello-list/post")
      (url "/hello/:id" hello-handler "hello-put/delete/get"))))
```


Run
-----------

Use ```app-run``` function to launch app.

```racket
(app-run
  routers
  #:port 8000
  #:log-file "access.log" ;your log file
  #:static-path (build-path (current-directory) "static") ;your static files dir
  #:static-url "static") ;your static url suffix

```


Customize Headers
-----------
You need require ```make-header``` from ```request-structs``` package.

```racket
(require
   (only-in web-server/http/request-structs
      make-header))

(define my-headers
  (list
  	 ...
    (make-header #"Access-Control-Allow-Origin" #"*")
    (make-header #"Access-Control-Allow-Methods" #"PUT, POST, GET, DELETE, PATCH, OPTIONS")))

```

Customize Response
-----------
Use ```response```fuction

```racket
(define (my-custom-rsp xml-doc)
  (response
    #:headers my-headers
    #:mime #"application/xml"
    #:body xml-doc))
```

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

    (define/public (put id)
      (jsonify (hash 'code 200 'msg "handle put" )))

    (define/public (delete id)
      (jsonify (hash 'code 200 'msg "handle delete" )))

    (super-new)))


(define index-handler
  (lambda (req)
    (jsonify (hash 'code 200 'msg "hello api" ))))


(define api-v1 (url-group "/api/v1")) ;define a url group

(define routers
  (urls
    (url "/" index-handler "handler with function")
    (api-v1
      (url "/hellos" hello-handler "hello-list/post")
      (url "/hello/:id" hello-handler "hello-put/delete/get"))))


(app-run
  routers
  #:port 8000
  #:log-file "access.log" ;your log file
  #:static-path (build-path (current-directory) "static") ;your static files dir
  #:static-url "static") ;your static url suffix

```

examples
----------
Very simple apps build with Vela in the [examples folder](https://github.com/nuty/vela/tree/master/examples).

...


License
-------
Licensed under the MIT License.