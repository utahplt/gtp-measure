#lang racket/base

(require racket/contract)
(provide
  (contract-out
    [current-tasks/targets
      (-> (listof gtp-measure-target/c) (listof gtp-measure-task/c))]

    [init-task
      (-> (listof gtp-measure-target/c) gtp-measure-config/c gtp-measure-task/c)]))

(require
  basedir
  gtp-measure/private/configure
  gtp-measure/private/parse
  racket/path
  racket/runtime-path)

;; =============================================================================

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
  (define task-dir (build-path (writable-config-dir #:program "gtp-measure") (number->string uid)))
  (if (directory-exists? task-dir)
    (raise-arguments-error 'init-task "(not/c directory-exists?)"
                           "directory" task-dir
                           "uid" uid)
    (make-directory task-dir)))

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
       (write-checklist new-targets new-task-dir)]
      [else
       (raise-arguments-error 'write-checklist "invalid target kind"
                              "kind" kind
                              "targets" targets)])))

(define (format-target-tag str i)
  (format "~a-~a" i str))

(define (write-typed-untyped-checklist tu-path filename config)
  (define num-units (typed-untyped->num-units tu-path))
  (define num-samples (config-ref config key:num-samples))
  (error 'notimpld))

(define (fresh-uid)
  (for/sum ([cd (in-list (list-config-dirs #:program "gtp-measure"))])
    (length (directory-list cd))))

(define normalize-targets
  (let ([cwd (normalize-path CWD)])
    (lambda (pre-targets)
      (sort
        (for/list ([t (in-list pre-targets)])
          (cons (path->bytes (normalize-path (car t) cwd)) (cdr t)))
        bytes<?
        #:key car))))

;; =============================================================================

(module+ test
  (require rackunit)

  (define TEST-DIR (simplify-path (build-path CWD "test")))
  (define F-TGT (build-path TEST-DIR "sample-file-target.rkt"))
  (define T-TGT (build-path TEST-DIR "sample-typed-untyped-target"))
  (define MY-TGT (build-path TEST-DIR "sample-test.rkt"))

  (test-case "normalize-targets"
    (let ([p0 F-TGT]
          [p1 T-TGT])
      (check-equal?
        (normalize-targets (list (cons p0 kind:file)))
        (list (cons (path->bytes (normalize-path p0)) kind:file)))
      (check-equal?
        (normalize-targets (list (cons p1 kind:file) (cons p0 kind:file)))
        (for/list ([p (in-list (list p0 p1))])
          (cons (path->bytes (normalize-path p)) kind:file)))))

  (test-case "write-targets"
    (define orig-tgts (list (cons (path->string F-TGT) kind:file)))
    (define filename (write-targets orig-tgts TEST-DIR))
    (define m-tgts (manifest->targets filename))
    (delete-file filename)
    (check-equal? orig-tgts m-tgts))

)
