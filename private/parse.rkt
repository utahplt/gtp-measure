#lang racket/base

;; From strings to data

(require racket/contract)
(provide
  GTP-MEASURE-TARGETS-ID

  kind:typed-untyped
  kind:file
  kind:manifest

  gtp-measure-kind/c

  valid-target?
  valid-target?/kind
  valid-file-target?
  valid-typed-untyped-target?
  valid-manifest-target?

  gtp-measure-target

  gtp-measure-target/c

  typed-untyped->num-components

  racket-filenames

  (contract-out
    [manifest->targets
      (-> (or/c module-path? resolved-module-path? module-path-index?) (listof gtp-measure-target/c))])

)

(require
  file/glob
  lang-file/read-lang-file
  racket/set
  syntax/parse
  (only-in racket/path
    file-name-from-path))

;; =============================================================================

(define GTP-MEASURE-TARGETS-ID 'gtp-measure-targets)

(define kind:typed-untyped 'typed-untyped)
(define kind:file 'file)
(define kind:manifest 'manifest)

(define gtp-measure-kind/c
  (or/c kind:typed-untyped kind:file kind:manifest))

(define gtp-measure-target/c
  (cons/c string? gtp-measure-kind/c))

(define (valid-target? str)
  (or (and (valid-file-target? str) kind:file)
      (and (valid-typed-untyped-target? str) kind:typed-untyped)
      (and (valid-manifest-target? str) kind:manifest)))

(define (valid-target?/kind str kind)
  (cond
   [(eq? kind kind:file)
    (valid-file-target? str)]
   [(eq? kind kind:typed-untyped)
    (valid-typed-untyped-target? str)]
   [(eq? kind kind:manifest)
    (valid-manifest-target? str)]))

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

(define (typed-untyped->num-components tu-dir)
  (set-count (racket-filenames (build-path tu-dir "typed"))))

(define (valid-manifest-target? str)
  (and (file-exists? str)
       (gtp-manifest-file? str)))

(define (gtp-manifest-file? str)
  (equal? (lang-file-lang str) "gtp-measure/manifest"))

(define-syntax-class gtp-measure-target
  #:commit
  #:attributes (string kind)
  (pattern tgt:str
    #:with string (syntax-e #'tgt)
    #:with kind #f)
  (pattern tgt:id
    #:with string (symbol->string (syntax-e #'tgt))
    #:with kind #f)
  (pattern (tgt:str . kind:id)
    #:with string (syntax-e #'tgt))
  (pattern (tgt:id . kind:id)
    #:with string (symbol->string (syntax-e #'tgt)))
)

(define (manifest->targets ps)
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require ps GTP-MEASURE-TARGETS-ID)))

;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit racket/runtime-path)

  (define-runtime-path CWD ".")
  (define TEST (simplify-path (build-path CWD "test")))
  (define F-TGT (simplify-path (build-path TEST "sample-file-target.rkt")))
  (define TU-TGT (simplify-path (build-path TEST "sample-typed-untyped-target")))
  (define M-TGT (simplify-path (build-path TEST "sample-manifest-target.rkt")))

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

  (test-case "valid-target?/kind"
    (check-true
      (valid-target?/kind F-TGT kind:file))
    (check-false
      (valid-target?/kind F-TGT kind:typed-untyped))
    (check-true
      (valid-target?/kind TU-TGT kind:typed-untyped))
    (check-false
      (valid-target?/kind TU-TGT kind:manifest)))

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

  (test-case "manifest->targets"
    (check-equal? (manifest->targets M-TGT)
                  (list (cons (path->string F-TGT) kind:file))))

  (test-case "gtp-measure-target"
    (define (parse stx)
      (syntax-parse stx
        [x:gtp-measure-target
         (list #'x.string #'x.kind)]
        [_
         #false]))

    (let ([v (list "sample-file-target.rkt" #f)])
      (check-equal?
        (map syntax-e (parse #'sample-file-target.rkt))
        v)
      (check-equal?
        (map syntax-e (parse #'"sample-file-target.rkt"))
        v))
    (check-equal?
      (map syntax-e (parse #`(sample-file-target.rkt . #,kind:file)))
      (list "sample-file-target.rkt" kind:file))
    (check-equal?
      (map syntax-e (parse #`("sample-file-target.rkt" . #,kind:file)))
      (list "sample-file-target.rkt" kind:file))

    (check-equal?
      (parse #'43)
      #false))
)
