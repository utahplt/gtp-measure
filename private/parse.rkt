#lang racket/base

;; From strings to data

(require racket/contract)
(provide
  GTP-MEASURE-TARGETS-ID
  GTP-MEASURE-CONFIG-ID

  kind:typed-untyped
  kind:file
  kind:manifest

  gtp-measure-kind/c

  valid-target?
  valid-target?/kind
  valid-file-target?
  valid-typed-untyped-target?
  valid-manifest-target?

  check-target/kind
  check-file-target
  check-typed-untyped-target
  check-manifest-target

  gtp-measure-target

  gtp-measure-target/c

  typed-untyped->num-components

  typed-untyped->num-configurations

  racket-filenames

  (contract-out
    [manifest->config
      (-> (or/c module-path? resolved-module-path? module-path-index?) (and/c hash? immutable?))]
    [manifest->targets
      (-> (or/c module-path? resolved-module-path? module-path-index?) (listof gtp-measure-target/c))]
    [string->time-limit
      (-> string? exact-nonnegative-integer?)]
    [hours->seconds
      (-> exact-nonnegative-integer? exact-nonnegative-integer?)]
    [minutes->seconds
      (-> exact-nonnegative-integer? exact-nonnegative-integer?)])

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
(define GTP-MEASURE-CONFIG-ID 'gtp-measure-config)

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

(define (check-target/kind str kind)
  (cond
   [(eq? kind kind:file)
    (check-file-target str)]
   [(eq? kind kind:typed-untyped)
    (check-typed-untyped-target str)]
   [(eq? kind kind:manifest)
    (check-manifest-target str)]))

(define (valid-file-target? str)
  (eq? #true (check-file-target str)))

(define (check-file-target str)
  (if (file-exists? str)
    (if (not (gtp-manifest-file? str))
      #true
      "expected non-manifest file")
    "file does not exist"))

(define (valid-typed-untyped-target? str)
  (eq? #true (check-typed-untyped-target str)))

(define (check-typed-untyped-target str)
  (if (directory-exists? str)
    (let ([u-dir (build-path str "untyped")]
          [t-dir (build-path str "typed")])
      (if (directory-exists? u-dir)
        (if (directory-exists? t-dir)
          (let ((u* (racket-filenames u-dir))
                (t* (racket-filenames t-dir)))
            (if (set=? u* t*)
              #true
              "typed/ and untyped/ sub-directories contain different files"))
          "typed/ sub-directory does not exist")
        "untyped/ sub-directory does not exist"))
    "directory does not exist"))

(define (racket-filenames dir)
  (for/set ([f (in-glob (build-path dir "*.rkt"))])
    (file-name-from-path f)))

(define (typed-untyped->num-components tu-dir)
  (set-count (racket-filenames (build-path tu-dir "typed"))))

(define (typed-untyped->num-configurations tu-dir)
  (expt 2 (typed-untyped->num-components tu-dir)))

(define (valid-manifest-target? str)
  (eq? #true (check-manifest-target str)))

(define (check-manifest-target str)
  (if (file-exists? str)
    (if (gtp-manifest-file? str)
      #true
      "expected a '#lang gtp-measure/manifest' file")
    "file does not exist"))

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

(define (manifest->config ps)
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require ps GTP-MEASURE-CONFIG-ID)))

(define string->time-limit
  (let ([t-rx #rx"^([0-9]+)(h|m|s|)$"])
    (lambda (str)
      (define m (regexp-match t-rx str))
      (define n (if m
                  (string->number (cadr m))
                  (raise-argument-error 'string->time-limit "string of 0-9 digits with optional unit suffix" str)))
      (case (caddr m)
        (("h") (hours->seconds n))
        (("m") (minutes->seconds n))
        (("s" "") n)
        (else (error 'string->time-limit "parsed unexpected suffix ~s from string ~s" (caddr m) str))))))

(define (hours->seconds n)
  (minutes->seconds (* 60 n)))

(define (minutes->seconds n)
  (* 60 n))

;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit racket/runtime-path)

  (define-runtime-path CWD ".")
  (define TEST (simplify-path (build-path CWD "test")))
  (define F-TGT (simplify-path (build-path TEST "sample-file-target.rkt")))
  (define TU-TGT (simplify-path (build-path TEST "sample-typed-untyped-target")))
  (define M-TGT (simplify-path (build-path TEST "sample-manifest-target.rkt")))
  (define M-TGT/config (simplify-path (build-path TEST "sample-manifest-target-config.rkt")))

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
      (check-equal? (set-count v) 6)
      (check set=? v (set (string->path "infinite-loop-file-target.rkt") (string->path "sample-file-target.rkt") (string->path "sample-manifest-target.rkt") (string->path "sample-manifest-target-config.rkt") (string->path "manifest1.rkt") (string->path "manifest2.rkt")))))

  (test-case "manifest->targets"
    (check-equal? (manifest->targets M-TGT)
                  (list (cons (path->string F-TGT) kind:file))))

  (test-case "manifest->config"
    (let ([h0 (manifest->config M-TGT)]
          [h1 (manifest->config M-TGT/config)])
      (check-pred hash? h0)
      (check-pred hash? h1)
      (check-equal? 0 (hash-count h0))
      (check-equal? 1 (hash-count h1))))

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

  (test-case "string->time-limit"
    (check-equal? (minutes->seconds 4)
                  240)
    (check-equal? (hours->seconds 1)
                  3600)
    (check-equal? (string->time-limit "0")
                  0)
    (check-equal? (string->time-limit "8s")
                  8)
    (check-equal? (string->time-limit "2m")
                  120)
    (check-equal? (string->time-limit "3h")
                  10800)
    (check-exn exn:fail:contract?
      (lambda () (string->time-limit "235ms"))))
)
