#lang info
(define collection "gtp-measure")
(define deps '(
  "at-exp-lib"
  "base"
  "basedir"
  "gtp-util"
  "lang-file"
  "scribble-lib"
  ("sandbox-lib" #:version "1.1")))
(define build-deps '(
  "rackunit-lib"
  "racket-doc"
  "scribble-doc"
  "basedir"
  "require-typed-check"
  "typed-racket-doc"
  "typed-racket-lib" ;; why?
))
(define pkg-desc "Benchmark harness")
(define version "1.1")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/gtp-measure.scrbl" ())))
(define raco-commands '(("gtp-measure" (submod gtp-measure/private/raco main) "Collect performance data" #f)))
(define compile-omit-paths '("private/test"))
(define test-omit-paths '("private/test"))
