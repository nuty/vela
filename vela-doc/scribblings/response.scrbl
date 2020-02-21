#lang scribble/manual
@title[#:style '(toc)]{Response}



@defproc[(argument 
                    [name (listof argument?)]
                    [argument-lst (listof argument?)]
                    [argumen(listof argumaent?)])
         sandwich?]{
  Returns arguments parse function.
}

@defproc[(arguments [argument-lst (listof argument?)])
         sandwich?]{
  Returns arguments parse function.
}