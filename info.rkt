#lang info
(define collection "gtp-measure")
(define deps '("base" "lang-file" "scribble-lib" "basedir" "gtp-util"))
(define build-deps '(
  "rackunit-lib"
  "racket-doc"
  "scribble-doc"
  "basedir"
  "require-typed-check"
  "typed-racket-doc"
  "typed-racket-lib" ;; why?
))
(define pkg-desc "For measuring")
(define version "0.1")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/gtp-measure.scrbl" ())))
(define raco-commands '(("gtp-measure" (submod gtp-measure/private/raco main) "Collect performance data" #f)))
(define compile-omit-paths '("private/test"))
(define test-omit-paths '("private/test"))
