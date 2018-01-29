#lang racket/base

;; 

(require racket/contract)
(provide
  kind:typed-untyped
  kind:file
  kind:manifest

  gtp-measure-kind/c

  valid-target?
  valid-file-target?
  valid-typed-untyped-target?
  valid-manifest-target?

  gtp-measure-target
)

(require
  file/glob
  lang-file/read-lang-file
  racket/set
  syntax/parse
  (only-in racket/path
    file-name-from-path))

;; =============================================================================

(define kind:typed-untyped 'typed-untyped)
(define kind:file 'file)
(define kind:manifest 'manifest)

(define gtp-measure-kind/c
  (or/c kind:typed-untyped kind:file kind:manifest))

(define (valid-target? str)
  (or (and (valid-file-target? str) kind:file)
      (and (valid-typed-untyped-target? str) kind:typed-untyped)
      (and (valid-manifest-target? str) kind:manifest)))

(define (valid-file-target? str)
  (and (file-exists? str)
       (not (gtp-manifest-file? str))))

(define (valid-typed-untyped-target? str)
  (and (directory-exists? str)
       (let ([u-dir (build-path str "untyped")]
             [t-dir (build-path str "typed")])
         (and (directory-exists? u-dir)
              (directory-exists? t-dir)
              (set=? (racket-filenames u-dir) (racket-filenames t-dir))))))

(define (racket-filenames dir)
  (for/set ([f (in-glob (build-path dir "*.rkt"))])
    (file-name-from-path f)))

(define (valid-manifest-target? str)
  (and (file-exists? str)
       (gtp-manifest-file? str)))

(define (gtp-manifest-file? str)
  (equal? (lang-file-lang str) "gtp-measure/manifest"))

(define-syntax-class gtp-measure-target
  #:commit
  #:attributes (string)
  (pattern tgt:str
    #:with string (syntax-e #'tgt)
    #:when (valid-target? (syntax-e #'tgt)))
  (pattern tgt:id
    #:with string (symbol->string (syntax-e #'tgt))
    #:when (valid-target? (syntax-e #'string))))

;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit racket/runtime-path)

  (define-runtime-path CWD ".")
  (define TEST (build-path CWD "test"))
  (define F-TGT (build-path TEST "sample-file-target.rkt"))
  (define TU-TGT (build-path TEST "sample-typed-untyped-target"))
  (define M-TGT (build-path TEST "sample-manifest-target.rkt"))

  (test-case "valid-target?"
    (check-equal?
      (valid-target? F-TGT)
      kind:file)
    (check-equal?
      (valid-target? TU-TGT)
      kind:typed-untyped)
    (check-equal?
      (valid-target? M-TGT)
      kind:manifest))

  (test-case "valid-file-target?"
    (check-true
      (valid-file-target? F-TGT))
    (check-false
      (valid-file-target? TU-TGT))
    (check-false
      (valid-file-target? M-TGT)))

  (test-case "valid-typed-untyped-target?"
    (check-false
      (valid-typed-untyped-target? F-TGT))
    (check-true
      (valid-typed-untyped-target? TU-TGT))
    (check-false
      (valid-typed-untyped-target? M-TGT)))

  (test-case "valid-manifest-target?"
    (check-false
      (valid-manifest-target? F-TGT))
    (check-false
      (valid-manifest-target? TU-TGT))
    (check-true
      (valid-manifest-target? M-TGT)))

  (test-case "racket-filenames"
    (let ((v (racket-filenames TEST)))
      (check-equal? (set-count v) 2)
      (check set=? v (set (string->path "sample-file-target.rkt") (string->path "sample-manifest-target.rkt")))))
)
