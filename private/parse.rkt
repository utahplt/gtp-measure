#lang racket/base

;; 

(provide
  kind:typed-untyped
  kind:file
  kind:manifest

  valid-target?
  valid-file-target?
  valid-typed-untyped-target?
  valid-manifest-target?

  gtp-measure-target
)

(require
  lang-file/read-lang-file
  syntax/parse
  (only-in racket/set set=? for/set)
  file/glob)

;; =============================================================================

(define kind:typed-untyped 'typed-untyped)
(define kind:file 'file)
(define kind:manifest 'manifest)

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
              (set=?  (racket-files u-dir) (racket-files t-dir))))))

(define (racket-files dir)
  (for/set ([f (in-glob (build-path dir "*.rkt"))])
    f))

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

