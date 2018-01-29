#lang racket/base

(provide
  (rename-out [-#%module-begin #%module-begin]))

(require
  (for-syntax
    gtp-measure/private/parse
    racket/base
    syntax/parse))

;; =============================================================================

(define-syntax (-#%module-begin stx)
  (syntax-parse stx
   [(_ tgt*:gtp-measure-target ...)
    #'(#%module-begin
        (provide gtp-measure-targets)
        (define gtp-measure-targets '((tgt*.string . tgt*.kind) ...)))]))

(module* reader syntax/module-reader
  gtp-measure/manifest
  #:read read
  #:read-syntax read-syntax)
