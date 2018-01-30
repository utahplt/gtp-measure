#lang racket/base

;; Defines a #lang for declaraing files/directories to benchmark

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
    #`(#%module-begin
        (provide #,GTP-MEASURE-TARGETS-ID)
        (require
          (only-in racket/path normalize-path path-only)
          (only-in gtp-measure/private/parse valid-target? valid-target?/kind))
        (define CWD
          (let ([p (variable-reference->module-source (#%variable-reference))])
            (if (path? p)
              (path-only p)
              (error 'gtp-measure/manifest "cannot find module source"))))
        (define #,GTP-MEASURE-TARGETS-ID
          (for/list ([pre-path (in-list '(tgt*.string ...))]
                     [pre-kind (in-list '(tgt*.kind ...))])
            (define p (normalize-path pre-path CWD))
            (if pre-kind
              (if (valid-target?/kind p pre-kind)
                (cons (path->string p) pre-kind)
                (raise-arguments-error 'gtp-measure/manifest "kind does not match target"
                                       "kind" pre-kind
                                       "target" pre-path))
              (let ([kind (valid-target? p)])
                (if kind
                  (cons (path->string p) kind)
                  (raise-arguments-error 'gtp-measure/manifest "invalid target"
                                         "target" pre-path)))))))]))

(module* reader syntax/module-reader
  gtp-measure/manifest
  #:read read
  #:read-syntax read-syntax)
