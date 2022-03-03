#lang racket/base

;; Command-line API
;; Run `raco gtp-measure --help` to see docs

(require
  gtp-measure/private/configure
  gtp-measure/private/parse
  gtp-measure/private/task
  gtp-measure/private/util
  gtp-measure/private/measure
  gtp-measure/private/check-pkg-deps
  racket/cmdline
  (only-in racket/path
    normalize-path)
  (only-in racket/port
    with-input-from-string))

;; =============================================================================

(define GTPM 'gtp-measure)

(define (make-target-assert valid? debug name)
  (lambda (str)
    (define p (normalize-path str))
    (if (valid? p)
      (path->string p)
      (raise-arguments-error
        GTPM
        (format "invalid ~s target" name)
        "input" str
        "reason" (debug p)))))

(define assert-valid-file
  (make-target-assert valid-file-target? check-file-target "file"))

(define assert-valid-typed-untyped
  (make-target-assert valid-typed-untyped-target? check-typed-untyped-target "typed-untyped"))

(define assert-valid-deep-shallow-untyped
  (make-target-assert valid-deep-shallow-untyped-target? check-deep-shallow-untyped-target "deep-shallow-untyped"))

(define assert-valid-manifest
  (make-target-assert valid-manifest-target? check-manifest-target "manifest"))

(define (read/ctc str ctc)
  (define v (with-input-from-string str read))
  (if (ctc v)
    v
    (raise-argument-error GTPM (format "~a" (object-name ctc)) str)))

(define (infer-target-type str)
  (or (valid-target? str)
      (raise-argument-error GTPM "valid-target?" str)))

(define (resume-task? task*)
  ;; TODO refactor for listof task (task*)
  (define short-msg
    (format "Resume ~a ? [Y/N]" task*))
  (define long-msg
    (format "Found existing task for targets:~n~s" task*))
  (displayln long-msg)
  (define resume?
    (let loop ()
      (displayln short-msg)
      (define x (parse-yes-or-no (read-line)))
      (or x (loop))))
  (case resume?
   [(Y) #true]
   [(N) #false]))

(define (parse-yes-or-no str)
  (case (string->symbol (string-upcase str))
   [(Y YE YES YOLO)
    'Y]
   [(N NO)
    'N]
   [else
    #false]))

(define (hash->immutable-hash h)
  (for/hash (((k v) (in-hash h)))
    (values k v)))

(define (ensure-trailing-slash str)
  (define L (string-length str))
  (if (zero? L)
    (raise-argument-error 'ensure-trailing-slash "non-empty-string?" str)
    (path->string (path->directory-path str))))

(define (ensure-directory d)
  (and (path-string? d)
       (void
         (cond
           [(file-exists? d)
            (raise-argument-error 'gtp-measure "(and/c path-string? (not/c file-exists?))" d)]
           [(directory-exists? d)
            (void)]
           [else
            (make-directory d)]))
       d))

(define (raco-parse argv)
  (define cmdline-config (make-hash))
  (define (set-config! k v)
    (hash-set! cmdline-config k v))
  (define *targets* (box '()))
  (define (add-target! tgt type)
    (set-box! *targets* (cons (cons tgt type) (unbox *targets*))))
  (define *mode* (box #f))
  (set-config! key:start-time (current-inexact-milliseconds))
  (set-config! key:argv (vector->list (current-command-line-arguments)))
  (command-line
    #:program "gtp-measure"
    #:argv argv
    #:once-any
    ;[("--clean")]
    [("--resume") task-dir "Resume a stopped task" (set-box! *mode* (cons 'resume (ensure-trailing-slash task-dir)))]
    [("--setup") "Setup task, but do not run" (set-box! *mode* 'setup)]
    #:multi
    [("-f" "--file") fname "target: file" (add-target! (assert-valid-file fname) kind:file)]
    [("-t" "--typed-untyped") dir "target: typed/untyped directory" (add-target! (assert-valid-typed-untyped dir) kind:typed-untyped)]
    [("--deep-shallow-untyped") dir "target: deep-shallow-untyped directory" (add-target! (assert-valid-deep-shallow-untyped dir) kind:deep-shallow-untyped)]
    [("-m" "--manifest") manifest "target: manifest" (add-target! (assert-valid-manifest manifest) kind:manifest)]
    #:once-each
    [("-i" "--iterations") iters "Number of iterations" (set-config! key:iterations (read/ctc iters exact-positive-integer?))]
    [("--bin") dir "Binaries directory" (set-config! key:bin dir)]
    [("--entry-point") main "Name of file to run (for typed/untyped targets)" (set-config! key:entry-point main)]
    [("-c" "--cutoff") N "Max. number of components to measure exhaustively (vs. by sampling)" (set-config! key:cutoff (read/ctc N exact-nonnegative-integer?))]
    [("-S" "--sample-factor") sf "Determines sample size (sample-size = S * num-components)" (set-config! key:sample-factor (read/ctc sf exact-nonnegative-integer?))]
    [("-R" "--num-samples") ns "Number of samples" (set-config! key:num-samples (read/ctc ns exact-positive-integer?))]
    [("--max-config-time") time-limit "time limit to run all iterations for one config. (h, m, or s)" (set-config! key:time-limit (string->time-limit time-limit))]
    [("--warmup") iters "JIT warmup iterations" (set-config! key:jit-warmup (read/ctc iters exact-nonnegative-integer?))]
    [("--output") out-dir "Directory to store inputs and outputs" (set-config! key:working-directory (ensure-directory (path->string (path->complete-path out-dir))))]
    #:args other-targets
    (let ([cmdline-config (hash->immutable-hash cmdline-config)]
          [mode (unbox *mode*)])
      (cond
        [(eq? mode 'clean)
         (raise-user-error 'gtp-measure "--clean not implemented")]
        [(and (pair? mode) (eq? 'resume (car mode)))
         (define dir (cdr mode))
         (unless (gtp-measure-task-directory? dir)
           (raise-arguments-error GTPM "gtp-measure-task-directory?" "--resume" dir))
         (unless (and (null? other-targets)
                      (null? (unbox *targets*)))
           (log-gtp-measure-warning "ignoring command-line targets ~a" (append other-targets (unbox *targets*))))
         (define old-task (resume-task (path->string (path->complete-path dir))))
         ;; TODO this code is very similar to the "normal" case below
         (log-gtp-measure-info "prepared task ~a (~a programs to run)" old-task (task->num-unmeasured-programs old-task))
         (void
           (measure old-task)
           (summarize-results old-task))]
        [(or (eq? mode 'setup) (eq? mode #f))
         (define all-targets
           (reverse
             (for/fold ([acc (unbox *targets*)])
                       ([tgt (in-list other-targets)])
               (cons (cons (path->string (normalize-path tgt)) (infer-target-type tgt)) acc))))
         (cond
          [(null? all-targets)
           (log-gtp-measure-warning "no targets specified") ;; TODO should be an error, but don't want to print during unit tests
           (raco-parse '#("--help"))]
          [else
           (log-gtp-measure-info "resolved targets ~a" all-targets)
           (define config (init-config cmdline-config))
           (define new-task (init-task all-targets config))
           (log-gtp-measure-info "prepared task ~a (~a programs to run)" new-task (task->num-unmeasured-programs new-task))
           (if (eq? mode 'setup)
             (printf "Setup complete ~s~n" new-task)
             (void
               (for ((cfg (in-list (task->config* new-task))))
                 (check-pkg-deps (config-ref cfg key:bin) #:auto? #true))
               (measure new-task)
               (summarize-results new-task)))])]
        [else
         (raise-arguments-error GTPM "unrecognized mode" "mode" mode)]))))

(define (summarize-results t)
  (printf "Finished ~s~n" t)
  (printf "Results in '*.out' files in directory ~a~n" (task->directory t))
  (void))

;; =============================================================================

(module+ main
  (raco-parse (current-command-line-arguments)))

;; =============================================================================

(module+ test
  (require
    rackunit
    racket/string
    racket/runtime-path
    (only-in racket/port open-output-nowhere)
    (submod gtp-measure/private/util test))

  (define-runtime-path sample-task-dir "./test/sample-task/24")

  ;; TODO running this test calls `exit`
  #;(filesystem-test-case "raco-parse"
    (check-not-exn
      (lambda ()
        (parameterize ([current-output-port (open-output-nowhere)]
                       [current-error-port (open-output-nowhere)])
          (raco-parse '#())))))

  (test-case "resume-task"
    (define-values [test->f-in test->f-out] (make-pipe))
    (define-values [f->test-in f->test-out] (make-pipe))
    (parameterize ([current-output-port f->test-out]
                   [current-input-port test->f-in])
      (check-true
        (begin (displayln 'Y test->f-out) (resume-task? '(X))))
      (check-true
        (string-prefix? (read-line f->test-in) "Found existing task"))
      (void ;; prints the value "#<void>"
        (read-line f->test-in))
      (check-true
        (string-prefix? (read-line f->test-in) "Resume"))
      (check-false
        (begin (displayln 'N test->f-out) (resume-task? '(X))))
      (void))
    (close-input-port test->f-in)
    (close-input-port f->test-in)
    (close-output-port test->f-out)
    (close-output-port f->test-out)
    (void))

  (test-case "parse-yes-or-no"
    (check-equal? (parse-yes-or-no "Y") 'Y)
    (check-equal? (parse-yes-or-no "yes") 'Y)
    (check-equal? (parse-yes-or-no "n") 'N)
    (check-equal? (parse-yes-or-no "NO") 'N)
    (check-equal? (parse-yes-or-no "idk") #false)
    (check-equal? (parse-yes-or-no "") #false))

  (test-case "ensure-trailing-slash"
    (check-equal? (ensure-trailing-slash "a") "a/")
    (check-equal? (ensure-trailing-slash "a/") "a/")
    (check-equal? (ensure-trailing-slash "a/b") "a/b/")
    (check-equal? (ensure-trailing-slash "a/b/") "a/b/")
    (check-exn exn:fail:contract?
      (lambda () (ensure-trailing-slash ""))))

  #;(filesystem-test-case "trailing-slash"
    ;; runs entire task
    (define dir*
      (let ([str (path->string sample-task-dir)])
        (list str (string-append str "/"))))
    (parameterize ([current-output-port (open-output-nowhere)])
      (for ((dir (in-list dir*)))
        (check-not-exn
          (lambda ()
            (raco-parse (vector-immutable "--resume" dir)))))))
)
