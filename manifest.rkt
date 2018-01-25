#lang racket/base

(provide
  (rename-out [-#%module-begin #%module-begin]))

(require
  (for-syntax racket/base syntax/parse))

;; =============================================================================

(begin-for-syntax
  (define (manifest-target-exists? str)
    (or (file-exists? str) (directory-exists? str)))

  (define-syntax-class gtp-measure-target
    #:commit
    #:attributes (string)
    (pattern tgt:str
      #:with string (syntax-e #'tgt)
      #:when (manifest-target-exists? (syntax-e #'tgt)))
    (pattern tgt:id
      #:with string (symbol->string (syntax-e #'tgt))
      #:when (manifest-target-exists? (syntax-e #'string)))))

(define-syntax (-#%module-begin stx)
  (syntax-parse stx
   [(_ tgt*:gtp-measure-target ...)
    #'(#%module-begin
        (provide gtp-measure-targets gtp-measure-config)
        (define gtp-measure-config '#hash())
        (define gtp-measure-targets '(tgt*.string ...)))]))

(module* reader syntax/module-reader
  gtp-measure/manifest
  #:read read
  #:read-syntax read-syntax)
