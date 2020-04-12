#lang scribble/manual
@title[#:style '(toc)]{Response}

@defproc[#:link-target? #f
        (jsonify 
              [headers list?]
              [body any?]
            )
         can-be-response?]



@codeblock|{
  (jsonify "hello world!")

  (jsonify (hash 'msg "ok"))
}|

@defproc[#:link-target? #f
        (response 
          [code 200]
          [message "OK"]
          [seconds (current-seconds)]
          [mime string?]
          [headers list?]
          [body response-body]
          )
         can-be-response?]
