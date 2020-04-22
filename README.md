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

Define Handler
-----------
There two ways to define a handler.

Use ```handler%```

```racket
(define hello-handler
  (class handler%

    (define/public (get [id null])
      (define req (get-field request this))
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


Use middleware
-----------

Use middleware in ```url```  or ```url-group```.
Whether it is request middleware or response middleware. When it returns a can-be-response?, 
the entire request will be interrupted and the responsed by the middleware will be returned.


```racket
(require
  vela
  web-server/http/request-structs)

;; handler
(define (index req)
  (jsonify "hello!"))

;; middlewares
(define (request-middleware req)
  (jsonify "hi!"))

(define (response-middleware req resp)
  (jsonify "bye!"))

;; url groups
(define req-urls
  (url-group "/req" #:on-request (list request-middleware)))

(define resp-urls
  (url-group "/resp" #:on-response (list response-middleware)))

(define req-and-resp-urls
  (url-group "/both" #:on-request (list request-middleware) #:on-response (list response-middleware)))


;; routers
(define middles-test-routers
  (urls
    (url "/req" index  #:on-request (list request-middleware) "request-middleware")
    (url "/resp" index  #:on-response (list response-middleware) "response-middleware")

    (req-urls
      (url "/test" index "test-req"))

    (resp-urls
      (url "/test" index "test-resp"))

    (req-and-resp-urls
      (url "/test" index "test-req-and-resp"))))

```

request middleware should set a request argument is request context. response middleware must set request and response arguments.


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

examples
----------
Very simple apps build with Vela in the [examples folder](https://github.com/nuty/vela/tree/master/examples).


License
-------
Licensed under the MIT License.
