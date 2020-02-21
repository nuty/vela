#lang scribble/manual
@title[#:style '(toc)]{Web handler}

Vela also support setting the response code and response headers using multiple return values, as shown below:


@larger{
  Function based handler
}

Function based handler use to write condational html app. 

@(racketblock

(define index
  (lambda (req)
    (jsonify (hash 'code 200 'msg "hello world!" )))))


  @bold{
    @italic{or}
  }

@(racketblock
  (define (index req)
    (jsonify (hash 'code 200 'msg "hello world!" ))))


@larger{
  Class based handler
}

Class based handler use to build RESTful API. 

@(racketblock

(define hello-world%
  (class handler%

    (define/public (get [id null])
      (jsonify (hash 'code 200 'msg "handle get" )))

    (define/public (post)
      (jsonify (hash 'code 200 'msg "handle post" )))

    (define/public (put id)
      (jsonify (hash 'code 200 'msg "handle put" )))

    (define/public (delete id)
      (jsonify (hash 'code 200 'msg "handle delete" )))
   
    (define/public (patch)
      (jsonify (hash 'code 200 'msg "handle patch" )))

    (super-new)))

)



