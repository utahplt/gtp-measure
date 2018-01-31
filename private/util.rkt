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
  log-gtp-measure-debug)

(require
  file/glob
  gtp-util
  (only-in racket/file
    delete-directory/files)
  (only-in racket/path
    file-name-from-path))

;; =============================================================================

(define-logger gtp-measure)

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
