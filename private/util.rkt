#lang racket/base

;; TODO move some of this to a gtp-util package

(provide
  copy-file*
  copy-racket-file*

  enumerate

  gtp-measure-logger
  log-gtp-measure-fatal
  log-gtp-measure-error
  log-gtp-measure-warning
  log-gtp-measure-info
  log-gtp-measure-debug

  force/gtp-measure)

(require
  file/glob
  gtp-util
  (only-in racket/file
    delete-directory/files)
  (only-in racket/path
    file-name-from-path)
  (only-in racket/logging
    with-intercepted-logging)
  (for-syntax
    racket/base
    (only-in racket/syntax format-id)
    syntax/parse))

;; =============================================================================

(define-logger gtp-measure)

(define-syntax (make-log-interceptor stx)
  (syntax-parse stx
   [(_ ?base-name:id)
    #:with ?logger-id (format-id stx "~a-logger" (syntax-e #'?base-name))
    #'(lambda (thunk #:level [level 'info])
        (define inbox (make-hasheq '((debug . ()) (info . ()) (warning . ()) (error . ()) (fatal . ()))))
        (with-intercepted-logging
          (λ (l)
            (define lvl (vector-ref l 0))
            (define msg (vector-ref l 1))
            (when (eq? '?base-name (vector-ref l 3))
              (hash-set! inbox lvl (cons msg (hash-ref inbox lvl))))
            (void))
          thunk
          #:logger ?logger-id
          level)
      (for/hasheq ([(k v) (in-hash inbox)])
        (values k (reverse v))))]))

(define force/gtp-measure
  (make-log-interceptor gtp-measure))

;; =============================================================================

(module+ test
  (provide filesystem-test-case)

  (require
    rackunit
    (for-syntax racket/base))

  (define-syntax (filesystem-test-case stx)
    (if (getenv "CI")
      #'(void)
      (with-syntax ([stuff (cdr (syntax-e stx))])
        #'(test-case . stuff))))

)
