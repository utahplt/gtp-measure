#lang racket/base

;; TODO move some of this to a gtp-util package

(provide
  copy-file*
  copy-racket-file*

  enumerate

  filesystem-test-case

  gtp-measure-logger
  log-gtp-measure-fatal
  log-gtp-measure-error
  log-gtp-measure-warning
  log-gtp-measure-info
  log-gtp-measure-debug)

(require
  file/glob
  rackunit
  (only-in racket/file
    delete-directory/files)
  (only-in racket/path
    file-name-from-path)
  (for-syntax
    racket/base))

;; =============================================================================

(define-logger gtp-measure)

(define (copy-file* src dst [pattern "*.*"])
  (for ([src-file (in-glob (build-path src pattern))])
    (define src-name (file-name-from-path src-file))
    (copy-file src-file (build-path dst src-name))))

(define (copy-racket-file* src dst)
  (copy-file* src dst "*.rkt"))

(define (enumerate x*)
  (for/list ([x (in-list x*)]
             [i (in-naturals)])
    (cons i x)))

(define-syntax (filesystem-test-case stx)
  (if (getenv "CI")
    #'(void)
    (with-syntax ([stuff (cdr (syntax-e stx))])
      #'(test-case . stuff))))

;; =============================================================================

(module+ test
  (require
    rackunit
    racket/runtime-path
    (only-in racket/set set-count)
    (only-in gtp-measure/private/parse racket-filenames))

  (define-runtime-path CWD ".")

  (define TEST-DIR (build-path CWD "test"))
  (define T-DIR (build-path TEST-DIR "sample-typed-untyped-target" "typed"))
  (define MY-DIR (build-path TEST-DIR "util-test"))

  (filesystem-test-case "copy-racket-file*"
    (when (directory-exists? MY-DIR)
      (delete-directory/files MY-DIR))
    (make-directory MY-DIR)
    (check-true
      (zero?  (set-count (racket-filenames MY-DIR))))
    (check-false
      (zero? (set-count (racket-filenames T-DIR))))
    (void
      (copy-racket-file* T-DIR MY-DIR))
    (check-false
      (zero? (set-count (racket-filenames MY-DIR))))
    (check-equal?
      (set-count (racket-filenames T-DIR))
      (set-count (racket-filenames MY-DIR)))
    ;; cleanup
    (delete-directory/files MY-DIR))

  (test-case "enumerate"
    (check-equal?
      (enumerate '())
      '())
    (check-equal?
      (enumerate '(A))
      '((0 . A)))
    (check-equal?
      (enumerate '(A B C))
      '((0 . A) (1 . B) (2 . C))))
)
