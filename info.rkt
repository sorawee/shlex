#lang info
(define collection "shlex")
(define deps '("parser-tools-lib"
               ["base" #:version "7.8"]))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/shlex.scrbl" ())))
(define pkg-desc "shlex for Racket: Simple lexical analysis")
(define version "0.0")
(define pkg-authors '(sorawee))
