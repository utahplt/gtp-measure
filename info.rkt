#lang info
(define collection "gtp-measure")
(define deps '("base" "lang-file" "scribble-lib"))
(define build-deps '("rackunit-lib" "racket-doc" "scribble-doc"))
(define pkg-desc "For measuring")
(define version "0.0")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/gtp-measure.scrbl" ())))
(define raco-commands '(("gtp-measure" (submod gtp-measure/private/raco main) "Collect performance data" #f)))