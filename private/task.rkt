#lang racket/base

;; Task = something we can benchmark,
;;  maybe a file,
;;  maybe a directory like for gradual typing performance,
;;  maybe a manifest that lists other things.
;;
;; This file deals with:
;; - data definition
;; - serialization
;; - checkpointing (divide a task into sub-tasks)
;; - running (i.e. what does it mean to run a task)

(require racket/contract)
(provide

  gtp-measure-task-directory?

  (contract-out
    [current-tasks/targets
      (-> (listof gtp-measure-target/c) (listof gtp-measure-task/c))]

    [init-task
      (-> (listof gtp-measure-target/c) gtp-measure-config/c gtp-measure-task/c)]

    [resume-task
      (-> gtp-measure-task-directory? gtp-measure-task/c)]

    [in-subtasks
      (-> gtp-measure-task/c any)]

    [subtask-run!
      (-> gtp-measure-subtask/c void?)]

    [task->directory
      (-> gtp-measure-task/c path-string?)]

    [task->total-programs
      (-> gtp-measure-task/c exact-nonnegative-integer?)]

    [task->num-unmeasured-programs
      (-> gtp-measure-task/c exact-nonnegative-integer?)]))

(require
  gtp-measure/private/configure
  gtp-measure/private/parse
  gtp-measure/private/util
  file/glob
  racket/file
  racket/path
  racket/set
  racket/runtime-path
  (only-in racket/system
    process)
  (only-in racket/list
    make-list)
  (only-in racket/string
    string-join
    string-replace)
  (only-in racket/port
    port->string
    port->lines)
  (only-in gtp-util
    bitstring?
    natural->bitstring
    filename-sort)
  (only-in racket/sequence
    sequence-map
    sequence->list)
  (only-in racket/pretty
    pretty-format))

;; =============================================================================

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
             (pretty-format (gtp-measure-task-targets t))
             (pretty-format (gtp-measure-task-config t)))
    (fprintf port "#<task:~a>" (gtp-measure-task-uid t))))

(struct gtp-measure-task [uid targets config]
  #:extra-constructor-name make-gtp-measure-task
  #:methods gen:custom-write [(define write-proc task-print)])

(define gtp-measure-task/c
  gtp-measure-task?)

(define (gtp-measure-task-directory? x)
  (and (path-string? x)
       (directory-exists? x)
       (file-exists? (build-path x MANIFEST.RKT))
       (file-exists? (build-path x CONFIG.RKTD))
       (equal? (normalize-path (path-only x)) (gtp-measure-data-dir))))

(define (current-tasks/targets pre-targets)
  (define targets (normalize-targets pre-targets))
  ;; TODO implement!
  '())

(define (init-task pre-targets config)
  (define uid (fresh-uid))
  (log-gtp-measure-debug "creating task ~a" uid)
  (define targets (normalize-targets pre-targets))
  (define task-dir (make-task-directory uid))
  (void
    (write-config config task-dir)
    (write-targets targets task-dir)
    (write-checklist targets config task-dir))
  (make-gtp-measure-task uid targets config))

(define (resume-task dir)
  (define uid (task-directory->uid dir))
  (define targets (task-directory->targets dir))
  (define config (directory->config dir))
  (make-gtp-measure-task uid targets config))

(define (task-directory->uid dir)
  (define p (file-name-from-path dir))
  (string->number (path->string p)))

(define (task-directory->targets dir)
  (manifest->targets (build-path dir MANIFEST.RKT)))

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
       (define new-targets (manifest->targets (string->path ps)))
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
  (define num-components (typed-untyped->num-components tu-path))
  (define exhaustive? (<= num-components (config-ref config key:cutoff)))
  (define num-configurations (expt 2 num-components))
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
            (displayln (natural->bitstring i #:bits num-components))))))
    (let ([num-samples (config-ref config key:num-samples)]
          [sample-size (* (config-ref config key:sample-factor) num-components)])
      (for ([sample-id (in-range num-samples)])
        (define filename
          (path-add-extension
            (path-add-extension base-filename (format "~a" sample-id) #".")
            #".in" #"_"))
        (with-output-to-file filename
          (lambda ()
            (for ((i (in-range sample-size)))
              (displayln (natural->bitstring (random num-configurations) #:bits num-components)))))))))

(define (fresh-uid)
  (+ 1 (length (directory-list (gtp-measure-data-dir)))))

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

(struct pre-subtask [])
(struct pre-file-subtask pre-subtask [in-file out-file]
  #:extra-constructor-name make-pre-file-subtask
  #:transparent)
(struct pre-typed-untyped-subtask pre-subtask [tu-dir config-dir in-file* out-file*]
  #:extra-constructor-name make-pre-typed-untyped-subtask
  #:transparent)

(define gtp-measure-subtask/c
  gtp-measure-subtask?)

(define (task->total-programs t)
  (task->count-programs t #false))

(define (task->num-unmeasured-programs t)
  (task->count-programs t #true))

(define (task->count-programs t skip-finished?)
  (for/sum ([pst (in-pre-subtasks t)])
    (cond
      [(pre-file-subtask? pst)
       (if (and skip-finished? (file-exists? (pre-file-subtask-in-file pst)))
         0
         1)]
      [(pre-typed-untyped-subtask? pst)
       (for/sum ([in-file (in-list (pre-typed-untyped-subtask-in-file* pst))]
                 [out-file (in-list (pre-typed-untyped-subtask-out-file* pst))])
         (if (and skip-finished? (file-exists? out-file))
           0
           (count-configurations in-file)))]
      [else
       (raise-arguments-error 'task->count-programs "undefined for subtask" "subtask" pst "original task" t)])))

(define (in-subtasks t)
  ;; TODO better to be lazy
  (define config (gtp-measure-task-config t))
  (for*/list ([pst (in-pre-subtasks t)]
              [st (in-list (pre-subtask->subtask* pst config))])
    st))

(define (in-pre-subtasks t)
  (define e-targets (enumerate (gtp-measure-task-targets t)))
  (define task-dir (task->directory t))
  (pre-subtasks/internal e-targets task-dir))

(define (pre-subtasks/internal targets task-dir)
  (define current-targets (box targets))
  (define current-subtasks (box '()))
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
                  (define out-file (file->outfile task-dir tgt target-index))
                  (list (make-pre-file-subtask tgt out-file))]
                 [(eq? tgt-kind kind:typed-untyped)
                  (define in-file*
                    (glob (build-path task-dir (string-append (format-target-tag tgt target-index) "*.in"))))
                  (define out-file*
                    (for/list ([f (in-list in-file*)])
                      (path-replace-extension f #".out")))
                  (define config-dir
                    (build-path task-dir (format-target-tag tgt target-index) CONFIG))
                  (list (make-pre-typed-untyped-subtask tgt config-dir in-file* out-file*))]
                 [(eq? tgt-kind kind:manifest)
                  ;; TODO this should be lazy too
                  (define new-targets (enumerate (manifest->targets (string->path tgt))))
                  (define new-task-dir (build-path task-dir (format-target-tag tgt target-index)))
                  (sequence->list (pre-subtasks/internal new-targets new-task-dir))]
                 [else
                  (raise-arguments-error 'gtp-measure "invalid kind" "kind" tgt-kind)]))
      (next-subtask!)]
     [else
      ;; no targets, no subtasks
      #false]))
  (in-producer next-subtask! #f))

(define (file->outfile task-dir tgt target-index)
  (path-add-extension
    (build-path task-dir (format-target-tag tgt target-index)) #".out" #"."))

(define (pre-subtask->subtask* st config)
  (cond
    [(pre-file-subtask? st)
     (define in-file (pre-file-subtask-in-file st))
     (define out-file (pre-file-subtask-out-file st))
     (if (file-exists? out-file)
       '()
       (file->subtask* in-file out-file config))]
    [(pre-typed-untyped-subtask? st)
     (define tu-dir (pre-typed-untyped-subtask-tu-dir st))
     (define config-dir (pre-typed-untyped-subtask-config-dir st))
     (define-values [in-file* out-file*]
       (for/lists (_1 _2)
                  ([in-file (in-list (pre-typed-untyped-subtask-in-file* st))]
                   [out-file (in-list (pre-typed-untyped-subtask-out-file* st))]
                   #:when (not (file-exists? out-file)))
         (values in-file out-file)))
     (typed-untyped->subtask* tu-dir config-dir in-file* out-file* config)]
    [else
     (raise-argument-error 'pre-subtask->subtask "pre-subtask?" 1 config st)]))

(define (file->subtask* in-file out-file config)
  (define thunk
    (make-file-timer in-file config))
  (list (make-gtp-measure-subtask out-file thunk)))

(define (typed-untyped->subtask* tu-dir configuration-dir in-file* out-file* config)
  (define entry-file (path->string (build-path configuration-dir (config-ref config key:entry-point))))
  (for/list ([in-file (in-list in-file*)]
             [out-file (in-list out-file*)])
    (define (thunk [out-port (current-output-port)])
      (with-input-from-file in-file
        (lambda ()
          (for ([configuration-id (in-lines)])
            (delete-compiled! tu-dir)
            (copy-configuration! configuration-id tu-dir configuration-dir)
            (define-values [configuration-in configuration-out] (make-pipe))
            (void
              ((make-file-timer entry-file config) configuration-out)
              (close-output-port configuration-out)
              (writeln (list configuration-id (port->lines configuration-in)) out-port)
              (close-input-port configuration-in))))))
    (make-gtp-measure-subtask out-file thunk)))

(define (delete-compiled! dir)
  (define compiled (build-path dir "compiled"))
  (delete-directory/files compiled #:must-exist? #f))

(define (copy-configuration! configuration-id src-dir dst-dir)
  (define t-dir (build-path src-dir TYPED))
  (define u-dir (build-path src-dir UNTYPED))
  (for ([t-file (filename-sort (set->list (racket-filenames t-dir)))]
        [u-file (filename-sort (set->list (racket-filenames u-dir)))]
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
      (parameterize ([current-directory (path-only filename)])
        (apply values (process cmd #:set-pwd? #true))))
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
        (displayln ln out-port))
      (void)]
     [else
      (log-gtp-measure-error "subprocess ~a terminated with exit code ~a" sub-pid exit-code)
      (define sub-err-str (port->string sub-err))
      (displayln (string-replace sub-err-str "\n" " ") out-port)
      (log-gtp-measure-error sub-err-str)
      (void)])
    (close-output-port sub-out)
    (close-input-port sub-in)
    (close-input-port sub-err)
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
        (raise-arguments-error 'gtp-measure (format "failed to find '~a' executable" str)
                               "directory" bin-dir))))
  (values (car bin*) (cadr bin*)))

(define (subtask-run! st)
  (define run! (gtp-measure-subtask-thunk st))
  (define outfile (gtp-measure-subtask-out st))
  (with-output-to-file outfile #:exists 'error run!))

(define (count-configurations filename)
  (with-input-from-file filename
    (lambda ()
      (for/sum ((ln (in-lines)))
        (if (bitstring? ln) 1 0)))))

;; =============================================================================

(module+ test
  (require
    rackunit
    racket/path
    (submod gtp-measure/private/util test)
    (only-in racket/port with-input-from-string))

  (define TEST-DIR (simplify-path (build-path CWD "test")))
  (define F-TGT (build-path TEST-DIR "sample-file-target.rkt"))
  (define T-TGT (build-path TEST-DIR "sample-typed-untyped-target"))
  (define M-TGT (build-path TEST-DIR "sample-manifest-target.rkt"))
  (define MY-TGT (build-path TEST-DIR "sample-test.rkt"))
  (define SAMPLE-TASK (build-path TEST-DIR "sample-task"))
  (define TASK-24 (build-path SAMPLE-TASK "24"))

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

  (filesystem-test-case "write-checklist"
    (define tgts (list (cons (path->string F-TGT) kind:file)
                       (cons (path->string T-TGT) kind:typed-untyped)
                       (cons (path->string M-TGT) kind:manifest)))
    (define config (init-config))
    (define task-dir (build-path TEST-DIR (format "test-dir-~a" (length (directory-list TEST-DIR)))))
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
        (for/list ((i (in-range 4))) (natural->bitstring i #:bits 2)))
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

  (filesystem-test-case "write-targets"
    (define orig-tgts (list (cons (path->string F-TGT) kind:file)))
    (define filename (write-targets orig-tgts TEST-DIR))
    (define m-tgts (manifest->targets filename))
    (delete-file filename)
    (check-equal? orig-tgts m-tgts))

  (filesystem-test-case "fresh-uid"
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

  (filesystem-test-case "file->subtask*"
    (define test-file (build-path TEST-DIR "test-file->subtask.txt"))
    (when (file-exists? test-file)
      (delete-file test-file))
    (define config
      (init-config (make-immutable-hash
                     (list (cons key:iterations 2)
                           (cons key:jit-warmup 2)))))
    (define st* (file->subtask* F-TGT test-file config))
    (check-equal? (length st*) 1)
    (define out-str*
      (begin
        (subtask-run! (car st*))
        (begin0
          (file->lines test-file)
          (delete-file test-file))))
    (check-equal? (length out-str*) (config-ref config key:iterations))
    (check-true (andmap time-line? out-str*)))

  (filesystem-test-case "copy-configuration!"
    (define configuration-dir (build-path TEST-DIR "sample-typed-untyped-configuration"))
    (unless (directory-exists? configuration-dir)
      (make-directory configuration-dir))
    (copy-configuration! "00" T-TGT configuration-dir)
    (check-pred file-exists?
      (build-path configuration-dir "main.rkt"))
    (check-equal? (length (directory-list configuration-dir)) 2))

  (filesystem-test-case "typed-untyped->subtask*"
    (define configuration-dir (build-path TEST-DIR "sample-typed-untyped-configuration"))
    (unless (directory-exists? configuration-dir)
      (make-directory configuration-dir))
    (define configuration*
      (for/list ([i (in-range 4)])
        (natural->bitstring i #:bits 2)))
    (define in-file
      (let ([p (build-path TEST-DIR "sample-typed-untyped.in")])
        (with-output-to-file p
          (lambda ()
            (for ((c (in-list configuration*)))
              (displayln c))))
        p))
    (define out-file
      (build-path TEST-DIR "sample-typed-untyped.out"))
    (define config
      (init-config (make-immutable-hash
                     (list (cons key:iterations 2)
                           (cons key:jit-warmup 0)))))
    (define st* (typed-untyped->subtask* T-TGT configuration-dir (list in-file) (list out-file) config))
    (check-equal? (length st*) 1)
    (define out-str*
      (begin (subtask-run! (car st*))
             (begin0
               (file->lines out-file)
               (delete-directory/files configuration-dir)
               (delete-file in-file)
               (delete-file out-file))))
    (check-equal? (length out-str*) 4)
    (for ((c (in-list configuration*))
          (str (in-list out-str*)))
      (define v
        (with-input-from-string str read))
      (check-pred list? v)
      (check-equal? (car v) c)
      (check-equal? (length (cadr v)) (config-ref config key:iterations))
      (check-true (andmap time-line? (cadr v)))))

  (filesystem-test-case "make-file-timer"
    (define test-file (build-path TEST-DIR "test-make-file-timer.txt"))
    (when (file-exists? test-file)
      (delete-file test-file))
    (define config
      (init-config (make-immutable-hash
                     (list (cons key:bin (path->string (path-only (system-racket-path))))
                           (cons key:iterations 2)
                           (cons key:jit-warmup 2)))))
    (define thunk (make-file-timer F-TGT config))
    (define out-str-1*
      (begin
        (call-with-output-file test-file thunk)
        (begin0
          (file->lines test-file)
          (delete-file test-file))))
    (define out-str-2*
      (begin
        (with-output-to-file test-file thunk)
        (begin0
          (file->lines test-file)
          (delete-file test-file))))
    (for ((out-str* (in-list (list out-str-1* out-str-2*))))
      (check-equal? (length out-str*) (config-ref config key:iterations))
      (check-equal? (length out-str*) (config-ref config key:iterations))
      (check-pred time-line? (car out-str*))
      (check-true (andmap time-line? out-str*))))

  (test-case "time-line?"
    (check-pred time-line?
      "cpu time: 1012 real time: 1009 gc time: 104")
    (check-pred time-line?
      "cpu time: 1012 real time: 1009 gc time: 104\n")
    (check-false
      (time-line? "yolo")))

  (filesystem-test-case "bin->rackets"
    (define racket-path (system-racket-path))
    (define racket-dir (path-only racket-path))
    (define-values [raco-bin racket-bin] (bin->rackets racket-dir))
    (check-equal? racket-bin (path->string racket-path))
    (check-equal? (path->string (file-name-from-path raco-bin)) "raco"))

  (filesystem-test-case "subtask-run!"
    (define message "everything a-ok")
    (define test-file (build-path TEST-DIR "subtask-run-test.txt"))
    (define (thunk)
      (display message))
    (define sample-subtask (make-gtp-measure-subtask test-file thunk))
    (define out-str
      (begin
        (when (file-exists? test-file)
          (delete-file test-file))
        (subtask-run! sample-subtask)
        (begin0
          (file->string test-file)
          (delete-file test-file))))
    (check-equal? out-str message))

  (filesystem-test-case "task->count-programs"
    (define tgts (list (cons (path->string F-TGT) kind:file)
                       (cons (path->string T-TGT) kind:typed-untyped)
                       (cons (path->string M-TGT) kind:manifest)))
    (define config (init-config))
    ;; modify the state of the filesystem
    (define-values [actual-total actual-unmeasured]
      (let ()
        (define t (init-task tgts config))
        (define task-dir (task->directory t))
        (with-output-to-file (file->outfile task-dir F-TGT 0)
          (lambda () (writeln "fake-runtime")))
        (define total (task->total-programs t))
        (define unmeasured (task->num-unmeasured-programs t))
        (delete-directory/files task-dir)
        (values total unmeasured)))
    (check-equal? actual-total (+ 1
                                  (typed-untyped->num-configurations T-TGT)
                                  1))
    (check-equal? actual-unmeasured
                  ;; TODO why -2 ?
                  (- actual-total 2)))

  (filesystem-test-case "count-configurations"
    (define CONFIGS '("01" "10"))
    (define actual
      (let ()
        (define tmpfile (build-path TEST-DIR "count-configurations.rktd"))
        (with-output-to-file tmpfile #:exists 'replace
          (lambda () (for-each displayln CONFIGS)))
        (begin0 (count-configurations tmpfile)
                (delete-file tmpfile))))
    (check-equal? actual (length CONFIGS)))

  (test-case "task-directory->uid"
    (check-equal? (task-directory->uid (build-path "1" "2")) 2)
    (check-equal? (task-directory->uid TASK-24) 24))

  ;; TODO should be OK to run on travis ... maybe https://github.com/racket/racket/pull/1947
  (filesystem-test-case "task-directory->targets"
    (let ([tgts (task-directory->targets TASK-24)])
      (check-equal? (length tgts) 1)))

  ;; TODO should be OK to run on travis ... maybe https://github.com/racket/racket/pull/1947
  (filesystem-test-case "resume-task"
    (let* ([t (resume-task TASK-24)]
           [uid (gtp-measure-task-uid t)]
           [tgts (gtp-measure-task-targets t)]
           [config (gtp-measure-task-config t)]
           [st* (for*/list ([pst (pre-subtasks/internal (enumerate tgts) TASK-24)]
                            [st (in-list (pre-subtask->subtask* pst config))])
                  st)])
      (check-equal? uid 24)
      (check-equal? (length st*) 1)))
)
