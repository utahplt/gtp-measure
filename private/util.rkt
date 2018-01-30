#lang racket/base

;; TODO move to gtp-util package

(provide
  copy-file*
  copy-racket-file*

  gtp-measure-logger
  log-gtp-measure-fatal
  log-gtp-measure-error
  log-gtp-measure-warning
  log-gtp-measure-info
  log-gtp-measure-debug)

(require
  file/glob
  (only-in racket/file
    delete-directory/files)
  (only-in racket/path
    file-name-from-path))

;; =============================================================================

(define-logger gtp-measure)

(define (copy-file* src dst [pattern "*.*"])
  (for ([src-file (in-glob (build-path src pattern))])
    (define src-name (file-name-from-path src-file))
    (copy-file src-file (build-path dst src-name))))

(define (copy-racket-file* src dst)
  (copy-file* src dst "*.rkt"))

;; =============================================================================

(module+ test
  (require rackunit racket/runtime-path)

  (define-runtime-path CWD ".")

  (define TEST-DIR (build-path CWD "test"))
  (define T-DIR (build-path TEST-DIR "sample-typed-untyped-target" "typed"))
  (define MY-DIR (build-path TEST-DIR "util-test"))

  (test-case "copy-racket-file*"
    (make-directory MY-DIR)
    (check-true
      (zero?  (length (directory-list MY-DIR))))
    (check-false
      (zero? (length (directory-list T-DIR))))
    (void
      (copy-racket-file* T-DIR MY-DIR))
    (check-false
      (zero? (length (directory-list MY-DIR))))
    (check-equal?
      (length (directory-list T-DIR))
      (length (directory-list MY-DIR)))
    ;; cleanup
    (delete-directory/files MY-DIR))
)
