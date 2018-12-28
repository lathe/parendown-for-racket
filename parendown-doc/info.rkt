#lang info

(define collection "parendown")

(define deps (list "base"))
(define build-deps (list "parendown-lib" "racket-doc" "scribble-lib"))

(define scribblings
  (list (list "scribblings/parendown.scrbl" (list))))
