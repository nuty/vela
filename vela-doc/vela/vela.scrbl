#lang scribble/manual
@(require scribble/eval
          (for-label racket
                     vela))
@title[#:style '(toc)]{Vela: Simple web framework}
@author[(author+email "(Rosso)" "answerw16@gmail.com")]


@larger{
  @bold{
   Overview
  }
}

Vela is a web application framework. It is designed to make getting started quick and easy, 
with the ability to scale up to complex applications. is began as simple wrapper around web-server  
the goal is become a useful Racket web application framework.


@larger{
  @bold{
    Features
  }
}


@itemlist[#:style 'ordered
    @item{@italic{Web handlers with Function or Class.}}
    @item{@italic{Friendly way to define URL routers.}}
    @item{@italic{Pluggable middlewares when request and response.}}
    @item{@italic{Request params check and collector.}}
    @item{@italic{Entirely on the racket webserver lib.}}
]




@section{Quickstart}

@larger{ 
  @bold{
   Installation
  }
}

To install Vela from the command line:


@codeblock|{
  raco pkg install vela
}|


A minimal app looks like this:

@filebox["app.rkt"]{
  @codeblock|{
  #lang racket

  (require vela)

  (define index
    (lambda (req)
      (jsonify (hash 'code 200 'msg "hello world!" ))))

  (define routers
    (urls
      (url "/" index "index")))

  (app-run routers #:port 8000)
  }|
}



@include-section["handler.scrbl"]
@include-section["url.scrbl"]
@include-section["response.scrbl"]
@include-section["app.scrbl"]
@include-section["params.scrbl"]
@include-section["middleware.scrbl"]