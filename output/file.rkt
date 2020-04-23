#lang racket/base

;; Defines a #lang for single-file output

;; TODO parse and validate the data

(provide
  (rename-out [-#%module-begin #%module-begin]))

(require
  (for-syntax
    racket/base
    syntax/parse))

;; -----------------------------------------------------------------------------

(define-syntax (-#%module-begin stx)
  (syntax-parse stx
   [(_ . data*)
    #`(#%module-begin '#,(format "~e" (syntax->datum #'data*)))]))

(module* reader syntax/module-reader
  gtp-measure/output/file
  #:read s:read
  #:read-syntax s:read-syntax
  (require (prefix-in s: scribble/reader)))
