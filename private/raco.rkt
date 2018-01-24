#lang racket/base

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "gtp-measure"
    #:args ()
    (raise-user-error 'not-implemented)))
