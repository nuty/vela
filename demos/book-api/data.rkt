#lang racket

(define books 
  (list
    (hasheq 'id 1 'title "Programming Erlang: Software for a Concurrent World (Pragmatic Programmers)" 'author "Joe Armstrong")
    (hasheq 'id 2 'title "Common LISP: A Gentle Introduction to Symbolic Computation" 'author "David S. Touretzky")
    (hasheq 'id 3 'title "Python Crash Course: A Hands-On, Project-Based Introduction to Programming" 'author "Eric Matthes")
    (hasheq 'id 4 'title "The Go Programming Language (Addison-Wesley Professional Computing Series) " 'author "Alan A. A. Donovan ")
    (hasheq 'id 5 'title "Head First Ruby: A Brain-Friendly Guide" 'author "Jay McGavren")))

(provide books)


; (require net/url)
; (require json)
; (define (get-json url)
;    (call/input-url (string->url url)
;                    get-pure-port
;                    (compose string->jsexpr port->string)))
; ;; Returns a parsed JSON expression
; (get-json "https://www.ifixit.com/api/2.0/guides/13470")
