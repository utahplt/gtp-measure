#lang racket/base

(require racket/contract)
(provide
  (contract-out
    [current-tasks/targets
      (-> (listof gtp-measure-target/c) (listof gtp-measure-task/c))]

    [init-task
      (-> (listof gtp-measure-target/c) gtp-measure-config/c gtp-measure-task/c)]

    [in-subtasks
      (-> gtp-measure-task/c any)]

    [subtask-run!
      (-> gtp-measure-subtask/c void?)]))

(require
  gtp-measure/private/configure
  gtp-measure/private/parse
  gtp-measure/private/util
  file/glob
  racket/file
  racket/path
  racket/runtime-path
  (only-in racket/system
    process)
  (only-in racket/list
    make-list)
  (only-in racket/string
    string-join)
  (only-in racket/port
    port->string
    port->lines)
  (only-in gtp-plot/util
    natural->bitstring))

;; =============================================================================

;; TODO need ... a public API for this
(define *MAX-UNITS* (make-parameter 12))

(define-runtime-path CWD ".")

(define MANIFEST.RKT "manifest.rkt")
(define CONFIG "configuration")
(define BASE "base")
(define BOTH "both")
(define TYPED "typed")
(define UNTYPED "untyped")

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
  (define task-dir (format-task-directory uid))
  (if (directory-exists? task-dir)
    (raise-arguments-error 'init-task "(not/c directory-exists?)"
                           "directory" task-dir
                           "uid" uid)
    (begin
      (make-directory task-dir)
      task-dir)))

(define (task->directory t)
  (define task-dir (format-task-directory (gtp-measure-task-uid t)))
  (if (directory-exists? task-dir)
    task-dir
    (raise-arguments-error 'task->directory "directory-exists?"
                           "directory" task-dir
                           "task" t)))

(define (format-task-directory uid)
  (build-path (gtp-measure-data-dir) (number->string uid)))

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
  (void ;; setup config/base/both directories
    (make-directory base-filename)
    (make-directory (build-path base-filename CONFIG))
    (let ([base (build-path tu-path BASE)])
      (when (directory-exists? base)
        (copy-directory/files base (build-path base-filename BASE))))
    (let ([both (build-path tu-path BOTH)])
      (when (directory-exists? both)
        (copy-racket-file* both (build-path base-filename CONFIG)))))
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

;; -----------------------------------------------------------------------------

(define (subtask-print st port write?)
  (fprintf port "#<subtask:~a>" (gtp-measure-subtask-out st)))

(struct gtp-measure-subtask [out thunk]
  #:extra-constructor-name make-gtp-measure-subtask
  #:methods gen:custom-write [(define write-proc subtask-print)])

(define gtp-measure-subtask/c
  gtp-measure-subtask?)

(define (in-subtasks t)
  (define current-targets
    (box
      (for/list ([tgt (in-list (gtp-measure-task-targets t))]
                 [i (in-naturals)])
        (cons i tgt))))
  (define current-subtasks (box '()))
  (define config (gtp-measure-task-config t))
  (define task-dir (task->directory t))
  (define (next-subtask!)
    (cond
     [(not (null? (unbox current-subtasks)))
      (define subtasks (unbox current-subtasks))
      (define st (car subtasks))
      (set-box! current-subtasks (cdr subtasks))
      st]
     [(not (null? (unbox current-targets)))
      (define targets (unbox current-targets))
      (define-values [target-index tgt tgt-kind]
        (let* ([index+x (car targets)]
               [index (car index+x)]
               [x (cdr index+x)])
          (values index (car x) (cdr x))))
      (set-box! current-targets (cdr targets))
      (set-box! current-subtasks
                (cond
                 [(eq? tgt-kind kind:file)
                  (define out-file
                    (path-add-extension (build-path task-dir (format-target-tag tgt target-index)) #".out" #"."))
                  (file->subtask* tgt out-file config)]
                 [(eq? tgt-kind kind:typed-untyped)
                  (define in-file*
                    (glob (build-path task-dir (string-append (format-target-tag tgt target-index) "*.in"))))
                  (define out-file*
                    (for/list ([f (in-list in-file*)])
                      (path-replace-extension f #".out")))
                  (define config-dir
                    (build-path task-dir (format-target-tag tgt target-index) CONFIG))
                  (typed-untyped->subtask* tgt config-dir in-file* out-file* config)]
                 [(eq? tgt-kind kind:manifest)
                  (raise-user-error 'not-implemented)
                  #;(manifest->subtask* tgt task-dir config)]
                 [else
                  (raise-arguments-error 'gtp-measure "invalid kind" "kind" tgt-kind)]))
      (next-subtask!)]
     [else
      ;; no targets, no subtasks
      #false]))
  (in-producer next-subtask! #f))

(define (file->subtask* in-file out-file config)
  (define thunk
    (make-file-timer in-file config))
  (list (make-gtp-measure-subtask out-file thunk)))

(define (typed-untyped->subtask* tu-dir config-dir in-file* out-file* config)
  (define entry-file (path->string (build-path config-dir (config-ref config key:entry-point))))
  (for/list ([in-file (in-list in-file*)]
             [out-file (in-list out-file*)])
    (define (thunk [out-port (current-output-port)])
      (with-input-from-file in-file
        (lambda ()
          (for ([configuration-id (in-lines)])
            (copy-configuration! configuration-id tu-dir config-dir)
            (define-values [configuration-in configuration-out] (make-pipe))
            (void
              (write configuration-id out-port)
              ((make-file-timer entry-file config) configuration-out)
              (close-output-port configuration-out)
              (writeln (port->lines configuration-in) out-port)
              (close-input-port configuration-in))))))
    (make-gtp-measure-subtask out-file thunk)))

(define (copy-configuration! configuration-id src-dir dst-dir)
  (define t-dir (build-path src-dir TYPED "*.rkt"))
  (define u-dir (build-path src-dir UNTYPED "*.rkt"))
  (for ([t-file (racket-filenames t-dir)]
        [u-file (racket-filenames u-dir)]
        [bit (in-string configuration-id)])
    (unless (equal? t-file u-file)
      (raise-arguments-error 'copy-configuration! "mis-matched filenames"
                             "untyped file" u-file
                             "typed file" t-file
                             "directory" src-dir))
    (copy-file (build-path (if (eq? #\1 bit) t-dir u-dir) t-file)
               (build-path dst-dir t-file)
               #true)))

(define (manifest->subtask* dirname task-dir config)
  (raise-user-error 'nope))

(define (make-file-timer filename config)
  (define-values [raco-bin racket-bin]
    (bin->rackets (config-ref config key:bin)))
  (define iterations (config-ref config key:iterations))
  (define jit-warmup (config-ref config key:jit-warmup))
  (define devnull ;; https://stackoverflow.com/a/313115/5237018
    (case (system-type 'os) ((windows) "NUL") (else "/dev/null")))
  (define cmd
    (let* ([compile-cmd (format "~a make -v ~a" raco-bin filename)]
           [run-cmd (format "~a ~a" racket-bin filename)]
           [ignore-cmd (format "~a > ~a" run-cmd devnull)])
    (string-join
      (append
        (list compile-cmd)
        (make-list jit-warmup ignore-cmd)
        (make-list iterations run-cmd))
      " && ")))
  (lambda ([out-port (current-output-port)])
    (define-values [sub-in sub-out sub-pid sub-err sub-control]
      (apply values (process cmd)))
    (log-gtp-measure-info "begin subprocess ~a" sub-pid)
    (define exit-code
      (let loop ()
        (sub-control 'wait)
        (or (sub-control 'exit-code)
            (begin (log-gtp-measure-warning "resume subprocess ~a" sub-pid)
                   (loop)))))
    (log-gtp-measure-info "end subprocess ~a" sub-pid)
    (cond
     [(zero? exit-code)
      (for ((ln (in-lines sub-in))
            #:when (time-line? ln))
        (writeln ln out-port))
      (void)]
     [else
      (log-gtp-measure-error "subprocess ~a terminated with exit code ~a" sub-pid exit-code)
      (log-gtp-measure-error (port->string sub-err))
      (void)])
    (close-output-port sub-out)
    (close-input-port sub-in)
    (close-output-port sub-err)
    (void)))

(define time-line?
  (let ([rx #rx"^cpu time:"])
    (lambda (ln)
      (regexp-match? rx ln))))

(define (bin->rackets bin-dir)
  (define bin*
    (for/list ([str (in-list '("raco" "racket"))])
      (define bin (build-path bin-dir str))
      (if (file-exists? bin)
        (path->string bin)
        (raise-arguments-error 'gtp-measure (format "failed to find '~a' executable")
                               "directory" bin-dir))))
  (values (car bin*) (cadr bin*)))

(define (subtask-run! st)
  (define run! (gtp-measure-subtask-thunk st))
  (define outfile (gtp-measure-subtask-out st))
  (with-output-to-file outfile #:exists 'error run!))

;; =============================================================================

(module+ test
  (require rackunit racket/set racket/path)

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
          [tu-dir (build-path task-dir "1-sample-typed-untyped-target")]
          [m-dir (build-path task-dir "2-sample-manifest-target")])
      (check-pred file-exists?
        tu-path)
      (check-equal?
        (file->lines tu-path)
        (for/list ((i (in-range 4))) (natural->bitstring i #:pad 2)))
      (check-pred directory-exists?
        tu-dir)
      (check-pred directory-exists?
        (build-path tu-dir CONFIG))
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

  (test-case "bin->rackets"
    (define racket-path (find-executable-path (find-system-path 'exec-file)))
    (define racket-dir (path-only racket-path))
    (define-values [raco-bin racket-bin] (bin->rackets racket-dir))
    (check-equal? racket-bin (path->string racket-path))
    (check-equal? (path->string (file-name-from-path raco-bin)) "raco"))

)
