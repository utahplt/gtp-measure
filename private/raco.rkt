#lang racket/base

;; TODO
;; - parse command-line options into configuration settings
;; - infer the kind of each target
;; - invoke "Stage 1 Configure" and "Stage 2 Measure" and "Stage 3 Summarize"

(require
  gtp-measure/private/configure
  gtp-measure/private/parse
  gtp-measure/private/task
  gtp-measure/private/measure)

;; =============================================================================

(define (assert-valid-file str)
  (if (valid-file-target? str)
    str
    (raise-argument-error 'gtp-measure "valid-file-target?" str)))

(define (assert-valid-typed-untyped str)
  (if (valid-typed-untyped-target? str)
    str
    (raise-argument-error 'gtp-measure "valid-typed-untyped-target?" str)))

(define (assert-valid-manifest str)
  (if (valid-manifest-target? str)
    str
    (raise-argument-error 'gtp-measure "valid-manifest-target?" str)))

(define (infer-target-type str)
  (or (valid-target? str)
      (raise-argument-error 'gtp-measure "valid-target?" str)))

(define (resume-task? task)
  (define short-msg
    (format "Resume ~a ? [Y/N]" task))
  (define long-msg
    (format "Found existing task for targets:~n~s~n" task))
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
    'N]))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (define cmdline-config (make-hash))
  (define (set-config! k v)
    (hash-set! cmdline-config k v))
  (define *targets* (box '()))
  (define (add-target! tgt type)
    (set-box! *targets* (cons (cons tgt type) (unbox *targets*))))
  (command-line
    #:program "gtp-measure"
    #:once-each
    [("-i" "--iterations") ni "Num. iterations" (set-config! key:iterations ni)]
    [("--bin") bin "Binaries directory" (set-config! key:bin bin)]
    [("--entry-point") m "Name of file to run (for typed/untyped targets)" (set-config! key:entry-point m)]
    ;;TODO;;[("-S" "--sample-size") ss "Sample size" (set-config! key:sample-size ss)]
    [("-R" "--num-samples") ns "Number of samples" (set-config! key:num-samples ns)]
    [("--warmup") ww "JIT warmup iterations" (set-config! key:jit-warmup ww)]
    ;; TODO add key for entering sampling mode? (if too many configs, do sampling ... 'max-exhaustive' ? ... could apply same idea to sampling, note if too large)
    #:multi
    [("-f" "--file") fname "target: file" (add-target! (assert-valid-file fname) kind:file)]
    [("-tu" "--typed-untyped") tu-fname "target: typed/untyped directory" (add-target! (assert-valid-typed-untyped tu-fname) kind:typed-untyped)]
    [("-m" "--manifest") manifest "target: manifest" (add-target! (assert-valid-manifest manifest) kind:manifest)]
    #:args (other-targets)
    (define all-targets
      (reverse
        (for/fold ([acc (unbox *targets*)])
                  ([tgt (in-list other-targets)])
          (cons (cons tgt (infer-target-type tgt)) acc))))
    (define old-task
      (resume-task all-targets))
    (if (and old-task (resume-task? old-task))
      (measure old-task)
      (let ([config (init-config cmdline-config)])
        (measure (init-task all-targets config))))))
