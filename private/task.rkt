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
      (-> gtp-measure-task/c exact-nonnegative-integer?)]

    [task->config*
      (-> gtp-measure-task/c (listof gtp-measure-config/c))]

    [make-progress-counter
      (->* [natural?] [string?] (-> natural? string?))]

    [bin->rackets
      (-> directory-exists? (values file-exists? file-exists?))]))

(require
  gtp-measure/private/configure
  gtp-measure/private/parse
  gtp-measure/private/util
  file/glob
  racket/file
  racket/path
  racket/set
  racket/runtime-path
  (only-in racket/sandbox
    exn:fail:resource?
    call-with-deep-time-limit)
  (only-in racket/system
    process*)
  (only-in racket/list
    make-list)
  (only-in racket/string
    string-join
    string-prefix?
    string-replace)
  (only-in racket/math
    natural?
    order-of-magnitude)
  (only-in racket/port
    port->string
    port->lines)
  (only-in gtp-util
    bitstring?
    natural->bitstring
    filename-sort
    path-string->string)
  (only-in racket/sequence
    sequence-map
    sequence->list)
  (only-in racket/format
    ~r)
  (only-in racket/pretty
    pretty-format))

;; =============================================================================

(define-runtime-path CWD ".")

(define MANIFEST.RKT "manifest.rkt")
(define CONFIG "configuration")
(define BASE "base")
(define BOTH "both")
(define TYPED "typed")
(define SHALLOW "shallow")
(define UNTYPED "untyped")

(define INPUT-EXTENSION #".in")
(define OUTPUT-EXTENSION #".out")
(define LANG-PREFIX "gtp-measure/output/")

(define *target-tag-width* (make-parameter 0))
;; *target-tag-width* = minimum length of prefix on output files,
;;  for example, helps pad `1-a.in` to `001-a.in`

(define (task-print t port write?)
  (if write?
    (fprintf port
             "Task ~a~n- targets : ~a~n- config : ~a~n- working directory : ~a~n"
             (gtp-measure-task-uid t)
             (pretty-format (gtp-measure-task-targets t))
             (pretty-format (gtp-measure-task-config t))
             (pretty-format (gtp-measure-task-dir t)))
    (fprintf port "#<task:~a>" (gtp-measure-task-uid t))))

(struct gtp-measure-task [uid targets config dir]
  #:extra-constructor-name make-gtp-measure-task
  #:methods gen:custom-write [(define write-proc task-print)])

(define gtp-measure-task/c
  gtp-measure-task?)

(define (gtp-measure-task-directory? x)
  (and (path-string? x)
       (directory-exists? x)
       (file-exists? (build-path x MANIFEST.RKT))
       (file-exists? (build-path x CONFIG.RKTD))
       (valid-parent-directory? x)))

(define (valid-parent-directory? x)
  #true
  #;(equal? (normalize-path (path-only x)) (gtp-measure-data-dir)))

(define (current-tasks/targets pre-targets)
  (define targets (normalize-targets pre-targets))
  ;; TODO implement!
  '())

(define (init-task pre-targets config)
  (define uid (fresh-uid config))
  (log-gtp-measure-debug "creating task ~a" uid)
  (define targets (normalize-targets pre-targets))
  (define task-dir (make-task-directory uid config))
  (void
    (write-config config task-dir)
    (write-targets targets task-dir)
    (write-checklist targets config task-dir))
  (make-gtp-measure-task uid targets config task-dir))

(define (resume-task dir)
  (define uid (task-directory->uid dir))
  (define targets (task-directory->targets dir))
  (define config (directory->config dir))
  (make-gtp-measure-task uid targets config dir))

(define (task-directory->uid dir)
  (define p (directory-name-from-path dir))
  (string->number p))

(define (directory-name-from-path p)
  (define-values [_base name mbd] (split-path p))
  (unless mbd
    (raise-argument-error 'directory-name-from-path "syntactic spec for a directory" p))
  (path-string->string name))

(define (task-directory->targets dir)
  (manifest->targets (build-path dir MANIFEST.RKT)))

(define (make-task-directory uid config)
  (define task-dir (format-task-directory uid config))
  (if (directory-exists? task-dir)
    (raise-arguments-error 'init-task "(not/c directory-exists?)"
                           "directory" task-dir
                           "uid" uid)
    (begin
      (make-directory task-dir)
      task-dir)))

(define (task->directory t)
  (gtp-measure-task-dir t))

(define (format-task-directory uid config)
  (build-path (config-ref config key:working-directory) (number->string uid)))

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
  (parameterize ((*target-tag-width* (make-tag-width (length targets))))
    (for ((tgt (in-list targets))
          (i (in-naturals)))
      (define-values [ps kind] (values (car tgt) (cdr tgt)))
      (cond
        [(eq? kind kind:file)
         (void)]
        [(eq? kind kind:typed-untyped)
         (define filename (build-path task-dir (format-target-tag ps i)))
         (write-typed-untyped-checklist ps filename config)]
        [(eq? kind kind:deep-shallow-untyped)
         (define filename (build-path task-dir (format-target-tag ps i)))
         (write-deep-shallow-untyped-checklist ps filename config)]
        [(eq? kind kind:manifest)
         (define m-path (string->path ps))
         (define new-targets (manifest->targets m-path))
         (define new-task-dir (build-path task-dir (format-target-tag ps i)))
         (define new-config (manifest-file-update-config m-path config))
         (make-directory new-task-dir)
         (write-checklist new-targets new-config new-task-dir)]
        [else
         (raise-arguments-error 'write-checklist "invalid target kind"
                                "kind" kind
                                "targets" targets)]))))

(define (make-tag-width n)
  (+ 1 (if (zero? n) 0 (order-of-magnitude n))))

(define (format-target-tag str i [pre-tag-width #f])
  (define tag-width (or pre-tag-width (*target-tag-width*)))
  (define prefix
    (let* ((p (number->string i))
           (chars-left (- tag-width (string-length p))))
      (if (< 0 chars-left)
        (string-append (make-string chars-left #\0) p)
        p)))
  (define suffix
    (path->string (file-name-from-path str)))
  (string-append prefix "-" suffix))

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
    (let ([filename (path-add-extension base-filename INPUT-EXTENSION #".")])
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
            INPUT-EXTENSION #"_"))
        (with-output-to-file filename
          (lambda ()
            (for ((i (in-range sample-size)))
              (displayln (natural->bitstring (random num-configurations) #:bits num-components)))))))))

(define (write-deep-shallow-untyped-checklist tu-path base-filename config)
  (define num-components (deep-shallow-untyped->num-components tu-path))
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
    (let ([filename (path-add-extension base-filename INPUT-EXTENSION #".")])
      (with-output-to-file filename #:exists 'error
        (lambda ()
          (displayln (natural->bitstring 0 #:bits num-components))
          (for ((i (in-range 1 num-configurations)))
            (define bs (natural->bitstring i #:bits num-components))
            (define num-hi (num-hi-bits bs))
            (for ((j (in-range (expt 2 num-hi))))
              (define sub-bs (string->list (natural->bitstring j #:bits num-hi)))
              (displayln
                (apply string
                       (for/list ((c (in-string bs)))
                         (cond
                          [(eq? #\0 c)
                           #\0]
                          [(and (eq? #\1 c) (eq? #\1 (car sub-bs)))
                           (set! sub-bs (cdr sub-bs))
                           #\2]
                          [(and (eq? #\1 c) (eq? #\0 (car sub-bs)))
                           (set! sub-bs (cdr sub-bs))
                           #\1]
                          [else
                           (raise-arguments-error 'deep-shallow-untyped "bad news making checklist")])))))))))
    (raise-argument-error 'deep-shallow-untyped "exhaustive")))

(define (num-hi-bits bs)
  (for/sum ((c (in-string bs)))
    (if (eq? #\1 c) 1 0)))

(define (fresh-uid config)
  (define dir (config-ref config key:working-directory))
  (define max-uid
    (for/fold ([acc 0])
              ([ps (in-list (directory-list dir))])
      (max acc (or (string->number (path->string ps)) 0))))
  (+ 1 max-uid))

(define normalize-targets
  (let ([cwd (normalize-path CWD)])
    (lambda (pre-targets)
      (sort
        (for/list ([t (in-list pre-targets)])
          (cons (path->string (normalize-path (car t) cwd)) (cdr t)))
        natural-string<?
        #:key (compose1 string->natural-str* car)
        #:cache-keys? #true))))

;; natural sort: like string<? but compare sequences of digits as numbers
(define (natural-string<? n0* n1*)
  (let loop ((n0* n0*) (n1* n1*))
    (cond
      [(null? n0*)
       #true]
      [(null? n1*)
       #false]
      [else
       (define v0 (car n0*))
       (define v1 (car n1*))
       (cond
         [(and (exact-integer? v0)
               (exact-integer? v1))
          (or (< v0 v1)
              (and (= v0 v1) (loop (cdr n0*) (cdr n1*))))]
         [else
          (define v0+ (if (exact-integer? v0) (number->string v0) v0))
          (define v1+ (if (exact-integer? v1) (number->string v1) v1))
          (or (string<? v0+ v1+)
              (and (string=? v0+ v1+) (loop (cdr n0*) (cdr n1*))))])])))

(define (string->natural-str* str)
  (define-values [rev-acc last]
    (for/fold ([acc '()]
               [prev 0])
              ([lo+hi (in-list (regexp-match-positions* #rx"[0-9]+" str))])
      (values
        (list*
          (string->number (substring str (car lo+hi) (cdr lo+hi)))
          (substring str prev (car lo+hi))
          acc)
        (cdr lo+hi))))
  (reverse (cons (substring str last) rev-acc)))

;; -----------------------------------------------------------------------------

(define (subtask-print st port write?)
  (fprintf port "#<subtask:~a>" (gtp-measure-subtask-out st)))

(struct gtp-measure-subtask [out thunk config]
  #:extra-constructor-name make-gtp-measure-subtask
  #:methods gen:custom-write [(define write-proc subtask-print)])

(struct pre-subtask [])
(struct pre-file-subtask pre-subtask [in-file out-file]
  #:extra-constructor-name make-pre-file-subtask
  #:transparent)
(struct pre-typed-untyped-subtask pre-subtask [tu-dir config-dir in-file* out-file*]
  #:extra-constructor-name make-pre-typed-untyped-subtask
  #:transparent)
(struct pre-deep-shallow-untyped-subtask pre-subtask [tu-dir config-dir in-file* out-file*]
  #:extra-constructor-name make-pre-deep-shallow-untyped-subtask
  #:transparent)
(struct pre-manifest-subtask pre-subtask [manifest-file pre-subtask*]
  #:extra-constructor-name make-pre-manifest-subtask
  #:transparent)

(define gtp-measure-subtask/c
  gtp-measure-subtask?)

(define (task->total-programs t)
  (task->count-programs t #false))

(define (task->num-unmeasured-programs t)
  (task->count-programs t #true))

(define (task->count-programs t skip-finished?)
  (for/sum ([pst (in-pre-subtasks t)])
    (pre-subtask->count-programs pst skip-finished?)))

(define (task->config* t)
  (define base-config (gtp-measure-task-config t))
  (for/fold ((acc (hash))
             #:result (hash-keys acc))
            ((pst (in-pre-subtasks t)))
    (cond
     [(or (pre-file-subtask? pst)
          (pre-typed-untyped-subtask? pst)
          (pre-deep-shallow-untyped-subtask? pst))
      (hash-set acc base-config #true)]
     [(pre-manifest-subtask? pst)
      (hash-set acc (pre-manifest-subtask-update-config pst base-config) #true)]
     [else
      (raise-argument-error 'task->config* "pre-subtask?" pst)])))

(define (pre-subtask->count-programs pst skip-finished?)
  (cond
    [(pre-file-subtask? pst)
     (if (and skip-finished? (file-exists? (pre-file-subtask-out-file pst)))
       0
       1)]
    [(pre-typed-untyped-subtask? pst)
     (for/sum ([in-file (in-list (pre-typed-untyped-subtask-in-file* pst))]
               [out-file (in-list (pre-typed-untyped-subtask-out-file* pst))])
       (typed-untyped->count-programs in-file out-file skip-finished?))]
    [(pre-deep-shallow-untyped-subtask? pst)
     (for/sum ([in-file (in-list (pre-deep-shallow-untyped-subtask-in-file* pst))]
               [out-file (in-list (pre-deep-shallow-untyped-subtask-out-file* pst))])
       (deep-shallow-untyped->count-programs in-file out-file skip-finished?))]
    [(pre-manifest-subtask? pst)
     (for/sum ([pst (in-list (pre-manifest-subtask-pre-subtask* pst))])
       (pre-subtask->count-programs pst skip-finished?))]
    [else
     (raise-arguments-error 'pre-subtask->count-programs "undefined for subtask" "subtask" pst)]))

(define (typed-untyped->count-programs in-file out-file skip-finished?)
  (define in-configs (count-configurations in-file))
  (if skip-finished?
    (let ([out-configs (count-data out-file)])
      (- in-configs out-configs))
    in-configs))

(define (deep-shallow-untyped->count-programs in-file out-file skip-finished?)
  (typed-untyped->count-programs in-file out-file skip-finished?))

(define (typed-untyped-finished? in-file out-file)
  (zero? (typed-untyped->count-programs in-file out-file #true)))

(define (deep-shallow-untyped-finished? in-file out-file)
  (typed-untyped-finished? in-file out-file))

(define (in-subtasks t)
  ;; TODO better to be lazy
  (define config (gtp-measure-task-config t))
  (for*/list ([pst (in-pre-subtasks t)]
              [st (in-list (pre-subtask->subtask* pst config))])
    st))

(define (in-pre-subtasks t)
  (define target* (gtp-measure-task-targets t))
  (define task-dir (task->directory t))
  (pre-subtasks/internal target* task-dir))

(define (pre-subtasks/internal target* task-dir)
  (parameterize ((*target-tag-width* (make-tag-width (length target*))))
    (for/list ([tgt (in-list target*)]
               [idx (in-naturals)])
      (target->pre-subtask idx tgt task-dir))))

(define (target->pre-subtask target-index target task-dir)
  (define tgt (car target))
  (define tgt-kind (cdr target))
  (cond
   [(eq? tgt-kind kind:file)
    (define out-file (file->outfile task-dir tgt target-index))
    (make-pre-file-subtask tgt out-file)]
   [(eq? tgt-kind kind:typed-untyped)
    (define in-file*
      (glob (build-path task-dir (string-append (format-target-tag tgt target-index) (format "*~a" INPUT-EXTENSION)))))
    (define out-file*
      (for/list ([f (in-list in-file*)])
        (path-replace-extension f OUTPUT-EXTENSION)))
    (define config-dir
      (build-path task-dir (format-target-tag tgt target-index) CONFIG))
    (make-pre-typed-untyped-subtask tgt config-dir in-file* out-file*)]
   [(eq? tgt-kind kind:deep-shallow-untyped)
    (define in-file*
      (glob (build-path task-dir (string-append (format-target-tag tgt target-index) (format "*~a" INPUT-EXTENSION)))))
    (define out-file*
      (for/list ([f (in-list in-file*)])
        (path-replace-extension f OUTPUT-EXTENSION)))
    (define config-dir
      (build-path task-dir (format-target-tag tgt target-index) CONFIG))
    (make-pre-deep-shallow-untyped-subtask tgt config-dir in-file* out-file*)]
   [(eq? tgt-kind kind:manifest)
    (define new-targets (manifest->targets (string->path tgt)))
    (define new-task-dir (build-path task-dir (format-target-tag tgt target-index)))
    (make-pre-manifest-subtask tgt (pre-subtasks/internal new-targets new-task-dir))]
   [else
    (raise-arguments-error 'target->pre-subtask "invalid kind" "kind" tgt-kind)]))

(define (file->outfile task-dir tgt target-index)
  (path-add-extension
    (build-path task-dir (format-target-tag tgt target-index)) OUTPUT-EXTENSION #"."))

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
                   #:unless (typed-untyped-finished? in-file out-file))
         (values in-file out-file)))
     (typed-untyped->subtask* tu-dir config-dir in-file* out-file* config)]
    [(pre-deep-shallow-untyped-subtask? st)
     (define tu-dir (pre-deep-shallow-untyped-subtask-tu-dir st))
     (define config-dir (pre-deep-shallow-untyped-subtask-config-dir st))
     (define-values [in-file* out-file*]
       (for/lists (_1 _2)
                  ([in-file (in-list (pre-deep-shallow-untyped-subtask-in-file* st))]
                   [out-file (in-list (pre-deep-shallow-untyped-subtask-out-file* st))]
                   #:unless (deep-shallow-untyped-finished? in-file out-file))
         (values in-file out-file)))
     (deep-shallow-untyped->subtask* tu-dir config-dir in-file* out-file* config)]
    [(pre-manifest-subtask? st)
     (define config+ (pre-manifest-subtask-update-config st config))
     (define pst* (pre-manifest-subtask-pre-subtask* st))
     (for*/list ([pst (in-list pst*)]
                 [new-st (in-list (pre-subtask->subtask* pst config+))])
       new-st)]
    [else
     (raise-argument-error 'pre-subtask->subtask "pre-subtask?" 1 config st)]))

(define (pre-manifest-subtask-update-config st old-config)
  (define m-file (pre-manifest-subtask-manifest-file st))
  (manifest-file-update-config (string->path m-file) old-config))

(define (manifest-file-update-config m-path old-config)
  (define m-config (manifest->config m-path))
  (update-config old-config m-config))

(define (file->subtask* in-file out-file config)
  (define thunk
    (let ([main (make-file-timer in-file config)])
      (lambda ([out-port (current-output-port)])
        (write-lang! out-port "file")
        (main))))
  (list (make-gtp-measure-subtask out-file thunk config)))

(define (typed-untyped->subtask* tu-dir configuration-dir in-file* out-file* config)
  (define entry-file (path->string (build-path configuration-dir (config-ref config key:entry-point))))
  (for/list ([in-file (in-list in-file*)]
             [out-file (in-list out-file*)])
    (define total-configs (count-configurations in-file))
    (define configs-done (count-data out-file))
    (define empty-out? (file-empty? out-file))
    (define (thunk [out-port (current-output-port)])
      (when (and (zero? configs-done) empty-out?)
        (write-lang! out-port "typed-untyped"))
      (define fmt (make-progress-counter total-configs "configuration"))
      (with-input-from-file in-file
        (lambda ()
          (void ;; skip already-done configs
            (for ([_ (in-lines)] [i (in-range configs-done)]) (void)))
          (for ([configuration-id (in-lines)]
                [cfg-i (in-naturals (+ 1 configs-done))])
            (log-gtp-measure-info "~a ~a" (fmt cfg-i) configuration-id)
            (copy-tu-configuration! configuration-id tu-dir configuration-dir)
            (define-values [configuration-in configuration-out] (make-pipe))
            (void
              ((make-file-timer entry-file config) configuration-out)
              (close-output-port configuration-out)
              (writeln (list configuration-id (port->lines configuration-in)) out-port)
              (close-input-port configuration-in))))))
    (make-gtp-measure-subtask out-file thunk config)))

(define (deep-shallow-untyped->subtask* tu-dir configuration-dir in-file* out-file* config)
  (define entry-file (path->string (build-path configuration-dir (config-ref config key:entry-point))))
  (for/list ([in-file (in-list in-file*)]
             [out-file (in-list out-file*)])
    (define total-configs (count-configurations in-file))
    (define configs-done (count-data out-file))
    (define empty-out? (file-empty? out-file))
    (define (thunk [out-port (current-output-port)])
      (when (and (zero? configs-done) empty-out?)
        (write-lang! out-port "deep-shallow-untyped"))
      (define fmt (make-progress-counter total-configs "configuration"))
      (with-input-from-file in-file
        (lambda ()
          (void ;; skip already-done configs
            (for ([_ (in-lines)] [i (in-range configs-done)]) (void)))
          (for ([configuration-id (in-lines)]
                [cfg-i (in-naturals (+ 1 configs-done))])
            (log-gtp-measure-info "~a ~a" (fmt cfg-i) configuration-id)
            (copy-dsu-configuration! configuration-id tu-dir configuration-dir)
            (define-values [configuration-in configuration-out] (make-pipe))
            (void
              ((make-file-timer entry-file config) configuration-out)
              (close-output-port configuration-out)
              (writeln (list configuration-id (port->lines configuration-in)) out-port)
              (close-input-port configuration-in))))))
    (make-gtp-measure-subtask out-file thunk config)))

(define (delete-compiled! dir)
  (define compiled (build-path dir "compiled"))
  (log-gtp-measure-debug "deleting zo folder ~a" compiled)
  (delete-directory/files compiled #:must-exist? #f))

(define (copy-tu-configuration! configuration-id src-dir dst-dir)
  (define t-dir (build-path src-dir TYPED))
  (define u-dir (build-path src-dir UNTYPED))
  (for ([t-file (filename-sort (set->list (racket-filenames t-dir)))]
        [u-file (filename-sort (set->list (racket-filenames u-dir)))]
        [bit (in-string configuration-id)])
    (unless (equal? t-file u-file)
      (raise-arguments-error 'copy-tu-configuration! "mis-matched filenames"
                             "untyped file" u-file
                             "typed file" t-file
                             "directory" src-dir))
    (copy-file (build-path (if (eq? #\1 bit) t-dir u-dir) t-file)
               (build-path dst-dir t-file)
               #true)))

(define (copy-dsu-configuration! configuration-id src-dir dst-dir)
  (define t-dir (build-path src-dir TYPED))
  (define u-dir (build-path src-dir UNTYPED))
  (define s-dir (build-path src-dir SHALLOW))
  (for ([t-file (filename-sort (set->list (racket-filenames t-dir)))]
        [s-file (filename-sort (set->list (racket-filenames s-dir)))]
        [u-file (filename-sort (set->list (racket-filenames u-dir)))]
        [bit (in-string configuration-id)])
    (unless (equal? t-file u-file)
      (raise-arguments-error 'copy-dsu-configuration! "mis-matched filenames"
                             "untyped file" u-file
                             "typed file" t-file
                             "directory" src-dir))
    (unless (equal? t-file s-file)
      (raise-arguments-error 'copy-dsu-configuration! "mis-matched filenames"
                             "typed file" t-file
                             "shallow file" s-file
                             "directory" src-dir))
    (copy-file (build-path (if (eq? #\1 bit) t-dir (if (eq? #\2 bit) s-dir u-dir)) t-file)
               (build-path dst-dir t-file)
               #true)))

(define (manifest->subtask* dirname task-dir config)
  (raise-user-error 'nope))

(define (make-file-timer filename config)
  (define-values [raco-bin racket-bin]
    (bin->rackets (config-ref config key:bin)))
  (define iterations (config-ref config key:iterations))
  (define jit-warmup (config-ref config key:jit-warmup))
  (define time-limit (config-ref config key:time-limit))
  (define devnull ;; https://stackoverflow.com/a/313115/5237018
    (case (system-type 'os) ((windows) "NUL") (else "/dev/null")))
  (define cmd*
    (let* ([compile-cmd (list raco-bin  "make" "-v" filename)]
           [run-cmd (list racket-bin filename)]
           [ignore-cmd run-cmd])
      (append
        (list compile-cmd)
        (make-list jit-warmup ignore-cmd)
        (make-list iterations run-cmd))))
  (lambda ([out-port (current-output-port)])
    (define file-dir (path-only filename))
    (delete-compiled! file-dir)
    (define *stop (box #f))
    (define *pid (box #f))
    (define *control (box #f))
    (define (run-all)
      (for ((cmd (in-list cmd*))
            (ii (in-naturals))
            #:unless (unbox *stop))
        (define ignore-output? (<= ii jit-warmup)) ;; ignore compile + warmup
        (define-values [sub-in sub-out sub-pid sub-err sub-control]
          (parameterize ([current-directory file-dir])
            (apply values (apply process* cmd #:set-pwd? #true))))
        (set-box! *pid sub-pid)
        (set-box! *control sub-control)
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
          (unless ignore-output?
            (for ((ln (in-lines sub-in))
                  #:when (time-line? ln))
              (displayln ln out-port)))
          (void)]
         [else
          (log-gtp-measure-error "subprocess ~a terminated with exit code ~a" sub-pid exit-code)
          (define sub-err-str (port->string sub-err))
          (displayln (string-replace sub-err-str "\n" " ") out-port)
          (log-gtp-measure-error sub-err-str)
          (set-box! *stop #true)
          (void)])
        (close-output-port sub-out)
        (close-input-port sub-in)
        (close-input-port sub-err)
        (void)))
    (if time-limit
      (let ()
        (define (handle-timeout ex)
          (log-gtp-measure-warning "subprocess ~a exceeded time limit (~as)" (unbox *pid) time-limit)
          (fprintf out-port "timeout ~a" (* time-limit 1000))
          ((unbox *control) 'kill)
          (void))
        (with-handlers ([exn:fail:resource? handle-timeout])
          (call-with-deep-time-limit time-limit run-all)))
      (run-all))))

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
  (with-output-to-file outfile #:exists 'append run!))

(define (count-configurations filename)
  (with-input-from-file filename
    (lambda ()
      (for/sum ((ln (in-lines)))
        (if (weak-bitstring? ln) 1 0)))))

(define (weak-bitstring? str)
  (for/and ((c (in-string str)))
    (or (eq? c #\0) (eq? c #\1) (eq? c #\2))))

(define (count-data filename)
  (if (file-exists? filename)
    (with-input-from-file filename
      (lambda ()
        (define *first (box #true))
        (for/sum ((ln (in-lines))
                  #:unless (begin0 (and (unbox *first) (string-prefix? ln "#lang"))
                                   (set-box! *first #false)))
          1)))
    0))

(define (file-empty? fn)
  (or (not (file-exists? fn))
      (zero? (file-size fn))))

(define (make-progress-counter total [unit-str #f])
  (define units (if unit-str (string-append unit-str " ") ""))
  (define num-digits (if (zero? total) 1 (+ 1 (order-of-magnitude total))))
  (define (fmt n) (~r n #:min-width num-digits #:pad-string " "))
  (lambda (i)
    (format "[~a~a/~a]" units (fmt i) total)))

(define (write-lang! out-port suffix)
  (fprintf out-port "#lang ~a~a~n" LANG-PREFIX suffix))

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
  (define D-TGT (build-path TEST-DIR "sample-deep-shallow-untyped-target"))
  (define M-TGT (build-path TEST-DIR "sample-manifest-target.rkt"))
  (define M-BIN-TGT (build-path TEST-DIR "sample-manifest-target-bin.rkt"))
  (define MY-TGT (build-path TEST-DIR "sample-test.rkt"))
  (define SAMPLE-TASK (build-path TEST-DIR "sample-task"))
  (define TASK-24 (build-path SAMPLE-TASK "24/"))
  (define TASK/MCFG
    ;; task with manifests that override the config
    (build-path SAMPLE-TASK "38/"))
  (define INFINITE-LOOP-TGT (build-path TEST-DIR "infinite-loop-file-target.rkt"))

  (test-case "task-print"
    (define t (make-gtp-measure-task "A" "B" "C" "D"))
    (define short-str (format "~a" t))
    (define long-str (format "~s" t))
    (check-regexp-match #rx"A" short-str)
    (check-false
      (regexp-match? #rx"B" short-str))
    (check-regexp-match #rx"A" long-str)
    (check-regexp-match #rx"B" long-str)
    (check-regexp-match #rx"C" long-str)
    (check-regexp-match #rx"D" long-str))

  (filesystem-test-case "write-checklist"
    (define tgts (list (cons (path->string F-TGT) kind:file)
                       (cons (path->string T-TGT) kind:typed-untyped)
                       (cons (path->string D-TGT) kind:deep-shallow-untyped)
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
          [dsu-dir (build-path task-dir "2-sample-deep-shallow-untyped-target")]
          [m-dir (build-path task-dir "3-sample-manifest-target.rkt")])
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
        dsu-dir)
      (check-pred directory-exists?
        (build-path dsu-dir CONFIG))
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
      "17-bar.rkt")
    (check-equal?
      (format-target-tag "foo/bar" 17)
      "17-bar")
    (check-equal?
      (format-target-tag "foo/bar.blah/baz" 17)
      "17-baz")
    (parameterize ((*target-tag-width* 3))
      (check-equal?
        (format-target-tag "foo/bar" 17)
        "017-bar"))
    (check-equal?
      (format-target-tag "foo/bar.blah/baz" 17 4)
      "0017-baz")
    (check-equal?
      (format-target-tag "foo/bar.blah/baz" 17 0)
      "17-baz")
    )

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
    (define config (init-config))
    (define uid (format "~a" (fresh-uid config)))
    (check-pred (lambda (x) (not (set-member? x uid)))
      (for*/list ([d (in-list (directory-list (config-ref config key:working-directory)))])
        (path->string d))))

  (test-case "normalize-targets"
    (define (make-expected . pth*)
      (for/list ((pth (in-list pth*)))
        (cons (path->string (normalize-path pth)) kind:file)))
    (let ([p0 F-TGT]
          [p1 T-TGT])
      (check-equal?
        (normalize-targets (list (cons p0 kind:file)))
        (make-expected p0))
      (check-equal?
        (normalize-targets (list (cons p1 kind:file) (cons p0 kind:file)))
        (make-expected p0 p1)))
    (let ([p0 (build-path TEST-DIR "2-tgt.rkt")]
          [p1 (build-path TEST-DIR "11-tgt.rkt")])
      (check-equal?
        (normalize-targets (list (cons p0 kind:file) (cons p1 kind:file)))
        (make-expected p0 p1)))
    (let ([p0 (build-path TEST-DIR "2-tgt.rkt")]
          [p1 (build-path TEST-DIR "tgt.rkt")])
      (check-equal?
        (normalize-targets (list (cons p0 kind:file) (cons p1 kind:file)))
        (make-expected p0 p1)))
    (let ([p0 (build-path TEST-DIR "2-tgt-11.rkt")]
          [p1 (build-path TEST-DIR "2-tgt-2.rkt")]
          [p2 (build-path TEST-DIR "3-tgt-1.rkt")])
      (check-equal?
        (normalize-targets (list (cons p0 kind:file) (cons p1 kind:file) (cons p2 kind:file)))
        (make-expected p1 p0 p2))))

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
    (check-equal? (length out-str*) (+ 1 (config-ref config key:iterations)))
    (check-equal? (car out-str*) "#lang gtp-measure/output/file")
    (check-true (andmap time-line? (cdr out-str*))))

  (filesystem-test-case "copy-tu-configuration!"
    (define configuration-dir (build-path TEST-DIR "sample-typed-untyped-configuration"))
    (unless (directory-exists? configuration-dir)
      (make-directory configuration-dir))
    (copy-tu-configuration! "00" T-TGT configuration-dir)
    (check-pred file-exists?
      (build-path configuration-dir "main.rkt"))
    (check-equal? (length (directory-list configuration-dir)) 2))

  (filesystem-test-case "copy-dsu-configuration!"
    (define configuration-dir (build-path TEST-DIR "sample-deep-shallow-untyped-configuration"))
    (unless (directory-exists? configuration-dir)
      (make-directory configuration-dir))
    (copy-dsu-configuration! "00" D-TGT configuration-dir)
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
    (define-values [log-hash out-str*]
      (let ([inbox (force/gtp-measure (lambda () (subtask-run! (car st*))) #:level 'debug)])
        (values inbox
                (begin0
                  (file->lines out-file)
                  (delete-directory/files configuration-dir)
                  (delete-file in-file)
                  (delete-file out-file)))))
    (check-equal? (length out-str*) 5)
    (check-equal?
      (for/sum ([msg (in-list (hash-ref log-hash 'debug))]
                #:when (regexp-match? #rx"deleting zo folder" msg))
        1)
      (- (length out-str*) 1)
      "missing evidence that 'zo' files deleted between configurations")
    (check-equal? (car out-str*) "#lang gtp-measure/output/typed-untyped")
    (for ((c (in-list configuration*))
          (str (in-list (cdr out-str*))))
      (define v
        (with-input-from-string str read))
      (check-pred list? v)
      (check-equal? (car v) c)
      (check-equal? (length (cadr v)) (config-ref config key:iterations))
      (check-true (andmap time-line? (cadr v)))))

  (filesystem-test-case "deep-shallow-untyped->subtask*"
    (define configuration-dir (build-path TEST-DIR "sample-deep-shallow-untyped-configuration"))
    (unless (directory-exists? configuration-dir)
      (make-directory configuration-dir))
    (define configuration*
      '("00" "01" "02" "10" "20" "11" "12" "21" "22"))
    (define in-file
      (let ([p (build-path TEST-DIR "sample-deep-shallow-untyped.in")])
        (with-output-to-file p
          (lambda ()
            (for ((c (in-list configuration*)))
              (displayln c))))
        p))
    (define out-file
      (build-path TEST-DIR "sample-deep-shallow-untyped.out"))
    (define config
      (init-config (make-immutable-hash
                     (list (cons key:iterations 2)
                           (cons key:jit-warmup 0)))))
    (define st* (deep-shallow-untyped->subtask* D-TGT configuration-dir (list in-file) (list out-file) config))
    (check-equal? (length st*) 1)
    (define-values [log-hash out-str*]
      (let ([inbox (force/gtp-measure (lambda () (subtask-run! (car st*))) #:level 'debug)])
        (values inbox
                (begin0
                  (file->lines out-file)
                  (delete-directory/files configuration-dir)
                  (delete-file in-file)
                  (delete-file out-file)))))
    (check-equal? (length out-str*) 10)
    (check-equal?
      (for/sum ([msg (in-list (hash-ref log-hash 'debug))]
                #:when (regexp-match? #rx"deleting zo folder" msg))
        1)
      (- (length out-str*) 1)
      "missing evidence that 'zo' files deleted between configurations")
    (check-equal? (car out-str*) "#lang gtp-measure/output/deep-shallow-untyped")
    (for ((c (in-list configuration*))
          (str (in-list (cdr out-str*))))
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
      (check-equal? (length out-str*) (config-ref config key:iterations)) ;; TODO
      (check-equal? (length out-str*) (config-ref config key:iterations))
      (check-pred time-line? (car out-str*))
      (check-true (andmap time-line? out-str*))))

  (filesystem-test-case "make-file-timer/timeout-pass"
    (define test-file (build-path TEST-DIR "test-make-file-timer.txt"))
    (define time-limit 5) ;; seconds
    (when (file-exists? test-file)
      (delete-file test-file))
    (define config
      (init-config (make-immutable-hash
                     (list (cons key:bin (path->string (path-only (system-racket-path))))
                           (cons key:time-limit time-limit)
                           (cons key:iterations 1)
                           (cons key:jit-warmup 1)))))
    (define thunk (make-file-timer F-TGT config))
    (define out-str*
      (begin
        (call-with-output-file test-file thunk)
        (begin0
          (file->lines test-file)
          (delete-file test-file))))
    (check-equal? (length out-str*) 1)) ;; TODO

  (filesystem-test-case "make-file-timer/timeout-fail"
    (define test-file (build-path TEST-DIR "test-make-file-timer.txt"))
    (define time-limit 1) ;; seconds
    (when (file-exists? test-file)
      (delete-file test-file))
    (define config
      (init-config (make-immutable-hash
                     (list (cons key:bin (path->string (path-only (system-racket-path))))
                           (cons key:time-limit time-limit)
                           (cons key:iterations 1)
                           (cons key:jit-warmup 1)))))
    (define thunk (make-file-timer INFINITE-LOOP-TGT config))
    (define out-str*
      (begin
        (call-with-output-file test-file thunk)
        (begin0
          (file->lines test-file)
          (delete-file test-file))))
    (check-equal? out-str* '("timeout 1000")))

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
    (check-equal? racket-bin (path->string (build-path (path-only racket-path) "racket")))
    (check-equal? (path->string (file-name-from-path raco-bin)) "raco"))

  (filesystem-test-case "subtask-run!"
    (define message "everything a-ok")
    (define test-file (build-path TEST-DIR "subtask-run-test.txt"))
    (define (thunk)
      (display message))
    (define config '#hash())
    (define sample-subtask (make-gtp-measure-subtask test-file thunk config))
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
                       (cons (path->string D-TGT) kind:deep-shallow-untyped)
                       (cons (path->string M-TGT) kind:manifest)))
    (define config (init-config))
    ;; modify the state of the filesystem
    (define-values [actual-total actual-unmeasured]
      (let ()
        (define t (init-task tgts config))
        (define task-dir (task->directory t))
        (with-output-to-file (file->outfile task-dir F-TGT 1)
          (lambda () (writeln "fake-runtime")))
        (define total (task->total-programs t))
        (define unmeasured (task->num-unmeasured-programs t))
        (delete-directory/files task-dir)
        (values total unmeasured)))
    (check-equal? actual-total (+ 1
                                  (typed-untyped->num-configurations T-TGT)
                                  (deep-shallow-untyped->num-configurations D-TGT)
                                  1))
    (check-equal? actual-unmeasured
                  (- actual-total 1)))

  (filesystem-test-case "task->config*"
    (define config (init-config))
    (define new-bin-str
      ;; valid path, but different from the default bin path
      (let* ((old-bin (config-ref config key:bin))
             (old-bin-name
              (let-values (((_base name _dir?) (split-path old-bin))) name)))
        (path->string (build-path old-bin ".." old-bin-name))))
    (define config-2 (config-set config key:bin new-bin-str))
    ;; basic task, expect 1 unique config
    (let ((t0 (init-task (list (cons (path->string F-TGT) kind:file)
                               (cons (path->string M-TGT) kind:manifest))
                         config)))
      (check-equal? (task->config* t0) (list config)))
    ;; task overrides key:bin, expect 2 configs if have two targets, 1 otherwise
    (let* ((old-data (file->string M-BIN-TGT))
           (_0 (with-output-to-file M-BIN-TGT #:exists 'replace
                 (lambda ()
                   (printf "#lang gtp-measure/manifest~n")
                   (printf "#:config #hash((bin . ~s))~n" new-bin-str)
                   (printf "sample-file-target.rkt~n"))))
           (cfg1* (task->config*
                    (init-task (list (cons (path->string M-BIN-TGT) kind:manifest))
                               config)))
           (cfg2* (task->config*
                    (init-task (list (cons (path->string F-TGT) kind:file)
                                     (cons (path->string M-BIN-TGT) kind:manifest))
                               config)))
           (_1 (with-output-to-file M-BIN-TGT #:exists 'replace
                 (lambda ()
                   (display old-data)))))
      (check-equal? cfg1* (list config-2))
      (check-pred (lambda (x) (member config x)) cfg2*)
      (check-pred (lambda (x) (member config-2 x)) cfg2*)
      (check-equal? (length cfg2*) 2)))

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
    (check-equal? (task-directory->uid (build-path "1" "2/")) 2)
    (check-equal? (task-directory->uid TASK-24) 24))

  (filesystem-test-case "task-directory->targets"
    (let ([tgts (task-directory->targets TASK-24)])
      (check-equal? (length tgts) 1)))

  (filesystem-test-case "resume-task"
    (let* ([t (resume-task TASK-24)]
           [uid (gtp-measure-task-uid t)]
           [tgts (gtp-measure-task-targets t)]
           [config (gtp-measure-task-config t)]
           [st* (for*/list ([pst (pre-subtasks/internal tgts TASK-24)]
                            [st (in-list (pre-subtask->subtask* pst config))])
                  st)])
      (check-equal? (length tgts) 1)
      (check-equal? uid 24)
      (check-equal? (length st*) 2))
    (let* ([t (resume-task TASK/MCFG)]
           [tgts (gtp-measure-task-targets t)]
           [orig-config (gtp-measure-task-config t)]
           [st* (for*/list ([pst (pre-subtasks/internal tgts TASK/MCFG)]
                            [st (in-list (pre-subtask->subtask* pst orig-config))])
                  st)])
      (check-equal? 2 (length st*))
      (for ((st (in-list st*))
            (iters (in-naturals 1)))
        (define cfg (gtp-measure-subtask-config st))
        (check-not-equal? orig-config cfg)
        (check-equal? (config-ref cfg key:iterations) iters))))

  (test-case "directory-name-from-path"
    (check-equal? (directory-name-from-path "a/") "a")
    (check-equal? (directory-name-from-path "a/b/") "b")
    (check-equal? (directory-name-from-path (string->path "a/")) "a")
    (check-equal? (directory-name-from-path (string->path "a/b/")) "b")
    (check-exn exn:fail:contract?
      (lambda () (directory-name-from-path "a"))))

  (test-case "make-progress-counter"
    (define ctr0 (make-progress-counter 10))
    (check-equal? (ctr0 0) "[ 0/10]")
    (check-equal? (ctr0 5) "[ 5/10]")
    (check-equal? (ctr0 10) "[10/10]")
    (check-equal? (ctr0 222) "[222/10]")

    (define ctrX (make-progress-counter 10 "X"))
    (check-equal? (ctrX 0) "[X  0/10]")
    (check-equal? (ctrX 2) "[X  2/10]")
    (check-equal? (ctrX 3) "[X  3/10]")
    (check-equal? (ctrX 10) "[X 10/10]")
    (check-equal? (ctrX 9001) "[X 9001/10]"))
)
