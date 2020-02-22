#lang info

(define collection 'multi)

(define deps '("base"))
(define build-deps '("base"
                     "racket-doc"
                     "data-doc"
                     "data-lib"
                     "vela-lib"
                     "scribble-lib"
                     "sandbox-lib"))

(define update-implies '("vela-lib"))

(define pkg-desc "documentation for \"vela\"")

(define pkg-authors '(rosso))