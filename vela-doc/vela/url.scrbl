#lang scribble/manual
@title[#:style '(toc)]{Routing}

Modern web applications use meaningful URLs to help users. Users are more likely to like a page and come back if the page uses a meaningful URL they can remember and use to directly visit a page.

bascly you need two function to define routers.


@defproc[#:link-target? #f
        (urls [url list?])
         (listof url)]


@defproc[#:link-target? #f
          (url 
            [path string?] 
            [handler procedure?] 
            [on-request list-of-procedure?] 
            [on-response list-of-procedure?]
            [end-point string?])
         (listof argument)]



@bold{
   url-group to grouping the routers
}


@defproc[#:link-target? #f
        (url-group 
            [prefix string?] 
            [on-request list-of-procedure?] 
            [on-response list-of-procedure?])
         (listof url)]

the on-request and on-request arguments are keyword arguments.

@bold{
    Example
}


@codeblock|{
(define api-v1 
  (url-group "/api/v1"))

(define routers
  (urls
    (url "/" index "index")

    (api-v1
      (url "/index" index)
      (url "/index1" index #:on-request "index1"))))
}|
