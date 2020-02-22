#lang scribble/manual
@title[#:style '(toc)]{Run App}



@defproc[(run-app 
        [routers urls?]
        [listen-ip "127.0.0.1"]
        [static-path path?]
        [static-url  string?]
        [log-file #f]
        )
        (serve/servlet)]