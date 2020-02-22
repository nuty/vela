#lang scribble/manual
@title[#:style '(toc)]{Response}

@defproc[(jsonify 
              [headers list?]
              [body any?]
            )
         can-be-response?]



@codeblock|{
  (jsonify "hello world!")

  (jsonify (hash 'msg "ok"))
}|

@defproc[(response 
                    [code 200]
                    [message "OK"]
                    [seconds (current-seconds)]
                    [mime string?]
                    [headers list?]
                    [body response-body]
                    )
         can-be-response?]
