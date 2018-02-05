#lang racket/base

;; Command-line API
;; Run `raco gtp-measure --help` to see docs

(require
  gtp-measure/private/configure
  gtp-measure/private/parse
  gtp-measure/private/task
  gtp-measure/private/util
  gtp-measure/private/measure
  gtp-measure/private/summarize
  racket/cmdline
  (only-in racket/path
    normalize-path)
  (only-in racket/port
    with-input-from-string))

;; =============================================================================

(define GTPM 'gtp-measure)

(define (assert-valid-file str)
  (define p (normalize-path str))
  (if (valid-file-target? p)
    (path->string p)
    (raise-argument-error GTPM "valid-file-target?" str)))

(define (assert-valid-typed-untyped str)
  (define p (normalize-path str))
  (if (valid-typed-untyped-target? p)
    (path->string p)
    (raise-argument-error GTPM "valid-typed-untyped-target?" str)))

(define (assert-valid-manifest str)
  (define p (normalize-path str))
  (if (valid-manifest-target? p)
    (path->string p)
    (raise-argument-error GTPM "valid-manifest-target?" str)))

(define (read/ctc str ctc)
  (define v (with-input-from-string str read))
  (if (ctc v)
    v
    (raise-argument-error GTPM (object-name ctc) str)))

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

(define (raco-parse argv)
  (define cmdline-config (make-hash))
  (define (set-config! k v)
    (hash-set! cmdline-config k v))
  (define *targets* (box '()))
  (define (add-target! tgt type)
    (set-box! *targets* (cons (cons tgt type) (unbox *targets*))))
  (define *mode* (box #f))
  (set-config! key:start-time (current-inexact-milliseconds))
  (set-config! key:argv (vector->immutable-vector (current-command-line-arguments)))
  (command-line
    #:program "gtp-measure"
    #:argv argv
    #:once-any
    ;[("--clean")]
    [("--resume") "Resume a stopped task" (set-box! *mode* 'resume)]
    [("--setup") "Setup task, but do not run" (set-box! *mode* 'setup)]
    #:multi
    [("-f" "--file") fname "target: file" (add-target! (assert-valid-file fname) kind:file)]
    [("-t" "--typed-untyped") dir "target: typed/untyped directory" (add-target! (assert-valid-typed-untyped dir) kind:typed-untyped)]
    [("-m" "--manifest") manifest "target: manifest" (add-target! (assert-valid-manifest manifest) kind:manifest)]
    #:once-each
    [("-i" "--iterations") iters "Number of iterations" (set-config! key:iterations (read/ctc iters exact-positive-integer?))]
    [("--bin") dir "Binaries directory" (set-config! key:bin dir)]
    [("--entry-point") main "Name of file to run (for typed/untyped targets)" (set-config! key:entry-point main)]
    [("-c" "--cutoff") N "Max. number of components to measure exhaustively (vs. by sampling)" (set-config! key:cutoff (read/ctc N exact-nonnegative-integer?))]
    [("-S" "--sample-factor") sf "Determines sample size (sample-size = S * num-components)" (set-config! key:sample-factor (read/ctc sf exact-nonnegative-integer?))]
    [("-R" "--num-samples") ns "Number of samples" (set-config! key:num-samples (read/ctc ns exact-positive-integer?))]
    [("--warmup") iters "JIT warmup iterations" (set-config! key:jit-warmup (read/ctc iters exact-positive-integer?))]
    #:args other-targets
    (let ([cmdline-config (hash->immutable-hash cmdline-config)]
          [mode (unbox *mode*)])
      (case mode
        [(clean)
         (raise-user-error 'gtp-measure "--clean not implemented")]
        [(resume)
         (raise-user-error 'gtp-measure "--resume not implemented")]
        [else
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
               (measure new-task)
               (printf "Finished ~s~n" new-task)
               (printf "Results in '*.out' files in directory ~a~n" (task->directory new-task))))])]))))

;; =============================================================================

(module+ main
  (raco-parse (current-command-line-arguments)))

;; =============================================================================

(module+ test
  (require
    rackunit
    racket/string
    (only-in racket/port open-output-nowhere)
    (submod gtp-measure/private/util test))

  (filesystem-test-case "raco-parse"
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

)
