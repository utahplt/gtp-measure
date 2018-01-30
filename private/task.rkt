#lang racket/base

(require racket/contract)
(provide
  (contract-out
    [current-tasks/targets
      (-> (listof gtp-measure-target/c) (listof gtp-measure-task/c))]

    [init-task
      (-> (listof gtp-measure-target/c) gtp-measure-config/c gtp-measure-task/c)]))

(require
  gtp-measure/private/configure
  gtp-measure/private/parse
  racket/path
  racket/runtime-path
  (only-in gtp-plot/util
    natural->bitstring))

;; =============================================================================

;; TODO need ... a public API for this
(define *MAX-UNITS* (make-parameter 12))

(define-runtime-path CWD ".")

(define MANIFEST.RKT "manifest.rkt")

(define (task-print t port write?)
  (if write?
    (fprintf port
             "Task ~a~n- targets : ~a~n- config : ~a~n"
             (gtp-measure-task-uid t)
             (gtp-measure-task-targets t)
             (gtp-measure-task-config t))
    (fprintf port "#<task:~a>" (gtp-measure-task-uid t))))

(struct gtp-measure-task [uid targets config]
  #:extra-constructor-name make-gtp-measure-task
  #:methods gen:custom-write [(define write-proc task-print)])

(define gtp-measure-task/c
  gtp-measure-task?)

(define gtp-measure-target/c
  (cons/c path-string? gtp-measure-kind/c))

(define (current-tasks/targets pre-targets)
  (define targets (normalize-targets pre-targets))
  ;; TODO implement!
  '())

(define (init-task pre-targets config)
  (define uid (fresh-uid))
  (define targets (normalize-targets pre-targets))
  (define task-dir (make-task-directory uid))
  (void
    (write-config config task-dir)
    (write-targets targets task-dir)
    (write-checklist targets config task-dir))
  (make-gtp-measure-task uid targets config))

(define (make-task-directory uid)
  (define task-dir
    (build-path (gtp-measure-data-dir) (number->string uid)))
  (if (directory-exists? task-dir)
    (raise-arguments-error 'init-task "(not/c directory-exists?)"
                           "directory" task-dir
                           "uid" uid)
    (begin
      (make-directory task-dir)
      task-dir)))

(define (write-config config task-dir)
  (config->directory config task-dir))

(define (write-targets targets task-dir)
  (define filename (build-path task-dir MANIFEST.RKT))
  (with-output-to-file filename #:exists 'error
    (lambda ()
      (displayln "#lang gtp-measure/manifest")
      (newline)
      (for-each writeln targets)))
  filename)

(define (write-checklist targets config task-dir)
  (for ((tgt (in-list targets))
        (i (in-naturals)))
    (define-values [ps kind] (values (car tgt) (cdr tgt)))
    (cond
      [(eq? kind kind:file)
       (void)]
      [(eq? kind kind:typed-untyped)
       (define filename (build-path task-dir (format-target-tag ps i)))
       (write-typed-untyped-checklist ps filename config)]
      [(eq? kind kind:manifest)
       (define new-targets (manifest->targets ps))
       (define new-task-dir (build-path task-dir (format-target-tag ps i)))
       (make-directory new-task-dir)
       (write-checklist new-targets config new-task-dir)]
      [else
       (raise-arguments-error 'write-checklist "invalid target kind"
                              "kind" kind
                              "targets" targets)])))

(define (format-target-tag str i)
  (format "~a-~a" i (path->string (path-remove-extension (file-name-from-path str)))))

(define (path-remove-extension ps)
  (path-replace-extension ps #""))

(define (write-typed-untyped-checklist tu-path base-filename config)
  (define num-units (typed-untyped->num-units tu-path))
  (define exhaustive? (<= num-units (*MAX-UNITS*)))
  (define num-configurations (expt 2 num-units))
  (if exhaustive?
    (let ([filename (path-add-extension base-filename #".in" #".")])
      (with-output-to-file filename #:exists 'error
        (lambda ()
          (for ((i (in-range num-configurations)))
            (displayln (natural->bitstring i #:pad num-units))))))
    (let ([num-samples (config-ref config key:num-samples)]
          [sample-size (* 10 num-units)]) ;; TODO abstract the sample-size
      (for ([sample-id (in-range num-samples)])
        (define filename
          (path-add-extension
            (path-add-extension base-filename (format "~a" sample-id) #".")
            #".in" #"_"))
        (with-output-to-file filename
          (lambda ()
            (for ((i (in-range sample-size)))
              (displayln (natural->bitstring (random num-configurations) #:pad num-units)))))))))

(define (fresh-uid)
  (length (directory-list (gtp-measure-data-dir))))

(define normalize-targets
  (let ([cwd (normalize-path CWD)])
    (lambda (pre-targets)
      (sort
        (for/list ([t (in-list pre-targets)])
          (cons (path->string (normalize-path (car t) cwd)) (cdr t)))
        string<?
        #:key car))))

;; =============================================================================

(module+ test
  (require rackunit racket/set racket/path racket/file)

  (define TEST-DIR (simplify-path (build-path CWD "test")))
  (define F-TGT (build-path TEST-DIR "sample-file-target.rkt"))
  (define T-TGT (build-path TEST-DIR "sample-typed-untyped-target"))
  (define M-TGT (build-path TEST-DIR "sample-manifest-target.rkt"))
  (define MY-TGT (build-path TEST-DIR "sample-test.rkt"))

  (test-case "task-print"
    (define t (make-gtp-measure-task "A" "B" "C"))
    (define short-str (format "~a" t))
    (define long-str (format "~s" t))
    (check-regexp-match #rx"A" short-str)
    (check-false
      (regexp-match? #rx"B" short-str))
    (check-regexp-match #rx"A" long-str)
    (check-regexp-match #rx"B" long-str)
    (check-regexp-match #rx"C" long-str))

  (test-case "write-checklist"
    (define tgts (list (cons F-TGT kind:file) (cons T-TGT kind:typed-untyped) (cons M-TGT kind:manifest)))
    (define config (init-config))
    (define task-dir (build-path TEST-DIR (format "~a" (fresh-uid))))
    ;; modify the state of the filesystem
    (void
      (make-directory task-dir)
      (write-checklist tgts config task-dir))
    ;; check modified state
    (let ([tu-path (build-path task-dir "1-sample-typed-untyped-target.in")]
          [m-dir (build-path task-dir "2-sample-manifest-target")])
      (check-pred file-exists?
        tu-path)
      (check-equal?
        (file->lines tu-path)
        (for/list ((i (in-range 4))) (natural->bitstring i #:pad 2)))
      (check-pred directory-exists?
        m-dir)
      (check-pred null?
        (directory-list m-dir)))
    ;; clean up
    (void
      (delete-directory/files task-dir)))

  (test-case "format-target-tag"
    (check-equal?
      (format-target-tag "foo/bar.rkt" 17)
      "17-bar"))

  (test-case "path-remove-extension"
    (check-equal?
      (path-remove-extension "foo/bar.rkt")
      (string->path "foo/bar")))

  (test-case "write-targets"
    (define orig-tgts (list (cons (path->string F-TGT) kind:file)))
    (define filename (write-targets orig-tgts TEST-DIR))
    (define m-tgts (manifest->targets filename))
    (delete-file filename)
    (check-equal? orig-tgts m-tgts))

  (test-case "fresh-uid"
    (define uid (format "~a" (fresh-uid)))
    (check-pred (lambda (x) (not (set-member? x uid)))
      (for*/list ([d (in-list (directory-list (gtp-measure-data-dir)))])
        (path->string d))))

  (test-case "normalize-targets"
    (let ([p0 F-TGT]
          [p1 T-TGT])
      (check-equal?
        (normalize-targets (list (cons p0 kind:file)))
        (list (cons (path->string (normalize-path p0)) kind:file)))
      (check-equal?
        (normalize-targets (list (cons p1 kind:file) (cons p0 kind:file)))
        (for/list ([p (in-list (list p0 p1))])
          (cons (path->string (normalize-path p)) kind:file)))))

)
