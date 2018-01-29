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

(define (task-print t port write?)
  (if write?
    (fprintf port
             "Task ~a~n- targets : ~a~n- config : ~a~n"
             (gtp-measure-task-uid t)
             (gtp-measure-task-targets t)
             (gtp-measure-task-config t))
    (fprintf port "#<task:~a>" (gtp-measure-task-uid t))))

(struct gtp-measure-task [uid targets config]
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
  ;; save config
  ;; save targets (as a manifest)
  ;; create TODO list for each target
  ;; create DONE list for each target (empty)
  (void))

(define (make-task-directory uid)
  (define task-dir (build-path (writable-config-dir #:program "gtp-measure") (number->string uid)))
  (if (directory-exists? task-dir)
    (raise-arguments-error 'init-task "(not/c directory-exists?)"
                           "directory" task-dir
                           "uid" uid)
    (make-directory task-dir)))

(define (fresh-uid)
  (for/sum ([cd (in-list (list-config-dirs #:program "gtp-measure"))])
    (length (directory-list cd))))

(define normalize-targets
  (let ([cwd (normalize-path CWD)])
    (lambda (pre-targets)
      (sort
        (for/list ([t (in-list pre-targets)])
          (cons (normalize-path (car t) cwd) (cdr t)))
        path<?
        #:key car))))


;; =============================================================================

(module+ test
  (require rackunit)

  (test-case "normalize-targets"
    (let ([p0 (build-path CWD "test" "sample-file-target.rktd")]
          [p1 (build-path CWD "test" "sample-typed-untyped-target")])
      (check-equal?
        (normalize-targets (list (cons p0 kind:file)))
        (list (cons (normalize-path p0) kind:file)))
      (check-equal?
        (normalize-targets (list (cons p1 kind:file) (cons p0 kind:file)))
        (for/list ([p (in-list (list p0 p1))])
          (cons (normalize-path p) kind:file)))))

)
