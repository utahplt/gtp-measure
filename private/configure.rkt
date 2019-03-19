#lang racket/base

;; Configuration options
;; - defines the keys / values / contracts for a configuration
;; - provides tools for working with a config,
;;   which is basically an immutable hash table but you don't need to know that

(require racket/contract)
(provide
  key:argv
  key:bin
  key:cutoff
  key:entry-point
  key:iterations
  key:jit-warmup
  key:num-samples
  key:sample-factor
  key:start-time
  key:time-limit
  key:working-directory

  CONFIG.RKTD

  config-ref

  gtp-measure-config/c

  system-racket-path

  (contract-out
    [config->directory
      (-> gtp-measure-config/c (and/c path-string? directory-exists?) void?)]
    [directory->config
      (-> (and/c path-string? directory-exists?) gtp-measure-config/c)]
    [init-config
     (->* [] [gtp-measure-config/c] gtp-measure-config/c)]
    [update-config
     (-> gtp-measure-config/c gtp-measure-config/c gtp-measure-config/c)]))

(require
  gtp-measure/private/util
  racket/set
  (only-in racket/file
    make-parent-directory*
    file->value)
  (only-in racket/path
    path-only)
  (only-in basedir
    writable-data-dir
    writable-config-file)
  (for-syntax racket/base syntax/parse))

;; =============================================================================

(define CONFIG.RKTD "config.rktd")

(define *config-spec* (box #f))
(define *default-config* (box #f))

(define (system-racket-path)
  (define exec-file (find-system-path 'exec-file))
  (define orig-dir (find-system-path 'orig-dir))
  (define pre-exec-path
    (if (absolute-path? exec-file)
      exec-file
      (build-path orig-dir exec-file)))
  (define exec-path (find-executable-path pre-exec-path))
  (or exec-path
      (raise-arguments-error 'system-racket-path
                             "cannot find absolute path to racket executable"
                             "(find-system-path 'exec-file)" exec-file
                             "(find-system-path 'orig-dir)" orig-dir
                             "(find-executable-path (build-path <orig-dir> <exec-file>))" exec-path)))

(define (gtp-measure-data-dir)
  (define ps (writable-data-dir #:program "gtp-measure"))
  (unless (directory-exists? ps)
    (make-parent-directory* ps)
    (make-directory ps))
  ps)

(define (assert-initialized! . bx*)
  (for ((bx (in-list bx*)))
    (unless (unbox bx)
      (error 'gtp-measure:configuration
             "gtp-measure configuration format did not initialize properly"
             (object-name bx) (unbox bx)))))

(define-syntax (define-gtp-config-format stx)
  (syntax-parse stx
   [(_ [k* v* ctc*] ...)
    #:with (k-sym* ...) (for/list ([k (in-list (syntax-e #'(k* ...)))])
                          (string->symbol (substring (symbol->string (syntax-e k)) 4)))
    #`(begin
        (define k* 'k-sym*) ...
        (set-box! *default-config* (make-immutable-hash (list (cons k* v*) ...)))
        (set-box! *config-spec* (list (cons k* ctc*) ...)))]))

(begin
  (define-gtp-config-format
    [key:entry-point "main.rkt" path-string?]
    [key:bin
      (path->string (path-only (system-racket-path)))
      (and/c string? directory-exists?)]
    [key:iterations       8  exact-positive-integer?]
    [key:num-samples     10  exact-positive-integer?]
    [key:jit-warmup       1  exact-nonnegative-integer?]
    [key:start-time       0  real?]
    [key:argv           '()  (listof string?)]
    [key:cutoff           9  exact-nonnegative-integer?]
    [key:sample-factor   10  exact-positive-integer?]
    [key:time-limit      #f  (or/c #f exact-nonnegative-integer?)]
    [key:working-directory
      (path->string (gtp-measure-data-dir))
      (and/c string? directory-exists? absolute-path?)])

  (assert-initialized! *default-config* *config-spec*)

  (define DEFAULT-CONFIG (unbox *default-config*))
  (define CONFIG-SPEC (unbox *config-spec*))

  (define has-gtp-config-keys?
    (let ([actual-keys (for/set ([kc (in-list CONFIG-SPEC)]) (car kc))])
      (lambda (h)
        (for/and ([k (in-hash-keys h)])
          (set-member? actual-keys k)))))

  (define has-gtp-config-values?
    (let ([key->contract (for/hash ([kc (in-list CONFIG-SPEC)]) (values (car kc) (cdr kc)))])
      (lambda (h)
        (for/and ([(k v) (in-hash h)])
          (define ctc (hash-ref key->contract k))
          (or (ctc v)
              (begin (log-gtp-measure-warning "key-value mismatch: key '~s' value ~s contract ~a" k v ctc)
                     #false))))))
)

(define gtp-measure-config/c
  (suggest/c
    (and/c hash?
           immutable?
           has-gtp-config-keys?
           has-gtp-config-values?)
    "suggestion" "subscribe to the `gtp-measure` logger at the `'warning` level"))

(define (config-ref cfg k)
  (hash-ref cfg k))

(define (init-config [cmdline-config #f])
  (update-config
    (update-config DEFAULT-CONFIG (file->config (gtp-measure-config-file)))
    cmdline-config))

(define (file->config filename)
  (file->value filename))

(define (directory->config dirname)
  (file->config (build-path dirname CONFIG.RKTD)))

(define (config->directory config dirname)
  (define filename (build-path dirname CONFIG.RKTD))
  (with-output-to-file filename #:exists 'error
    (lambda ()
      (writeln config))))

(define (gtp-measure-config-file)
  (define ps (writable-config-file CONFIG.RKTD #:program "gtp-measure"))
  (unless (file-exists? ps)
    (make-parent-directory* ps)
    (with-output-to-file ps
      (lambda () (writeln (make-immutable-hash)))))
  ps)

(define (update-config old-config new-config)
  (if new-config
    (hash-update* old-config new-config)
    old-config))

(define (hash-update* old-hash new-hash)
  (for/fold ([acc old-hash])
            ([(k v) (in-hash new-hash)])
    (hash-set acc k v)))

;; =============================================================================

(module* for-doc #f
  (provide DEFAULT-CONFIG))

(module+ test
  (require
    rackunit
    racket/runtime-path
    (only-in racket/file delete-directory/files)
    (submod gtp-measure/private/util test))

  (define CWD ".")

  (define TEST-DIR (build-path CWD "test"))

  (test-case "has-gtp-config-keys?"
    (check-pred has-gtp-config-keys?
      (make-immutable-hash))
    (check-pred has-gtp-config-keys?
      (make-immutable-hash (list (cons key:bin #f))))
    (check-pred has-gtp-config-keys?
      DEFAULT-CONFIG)

    (check-false
      (has-gtp-config-keys? (make-immutable-hash '((a . 1))))))

  (test-case "has-gtp-config-values?"
    (check-pred has-gtp-config-values?
      (make-immutable-hash))
    (check-pred has-gtp-config-values?
      (make-immutable-hash (list (cons key:argv '()))))
    (check-pred has-gtp-config-values?
      DEFAULT-CONFIG)

    (check-false
      (has-gtp-config-values? (make-immutable-hash (list (cons key:num-samples 0))))))

  (test-case "gtp-measure-config/c"
    (check-pred gtp-measure-config/c DEFAULT-CONFIG)

    (check-false
      (gtp-measure-config/c #f))
    (check-false
      (gtp-measure-config/c (make-hash)))
    (check-false
      (gtp-measure-config/c (make-immutable-hash '((a . 1)))))
    (check-false
      (gtp-measure-config/c (hash-set DEFAULT-CONFIG 'a 1))))

  (test-case "init-config"
    (let* ([secret-value 42]
           [k key:num-samples]
           [c (init-config (make-immutable-hash (list (cons k secret-value))))])
      (check-equal? (config-ref c k) secret-value)))

  (test-case "file->config"
    (let ([v (file->config (build-path CWD "test" "sample-config.rktd"))])
      (check-pred gtp-measure-config/c v)
      (check-equal? (config-ref v key:num-samples) 61)))

  (test-case "gtp-measure-config-file"
    (let ((v (gtp-measure-config-file)))
      (check-pred file-exists? v)))

  (filesystem-test-case "gtp-measure-data-dir"
    (check-pred directory-exists? (gtp-measure-data-dir)))

  (test-case "update-config"
    (check-equal?
      (update-config (make-immutable-hash '()) #f)
      (make-immutable-hash '()))
    (check-equal?
      (config-ref (update-config DEFAULT-CONFIG (make-immutable-hash (list (cons key:num-samples 999))))
                  key:num-samples)
      999))

  (test-case "hash-update*"
    (check-equal?
      (hash-update* (make-immutable-hash) (make-immutable-hash))
      (make-immutable-hash))
    (check-equal?
      (hash-update* (make-immutable-hash '((a . 1) (b . 2))) (make-immutable-hash))
      (make-immutable-hash '((a . 1) (b . 2))))
    (check-equal?
      (hash-update* (make-immutable-hash '((a . 1))) (make-immutable-hash '((b . 2))))
      (make-immutable-hash '((a . 1) (b . 2))))
    (check-equal?
      (hash-update* (make-immutable-hash '((a . 1) (b . 2))) (make-immutable-hash '((b . 3))))
      (make-immutable-hash '((a . 1) (b . 3)))))

  (filesystem-test-case "directory<->config"
    (check-equal?
      DEFAULT-CONFIG
      (let ()
        (define test-config (build-path TEST-DIR CONFIG.RKTD))
        (delete-directory/files test-config #:must-exist? #false)
        (config->directory DEFAULT-CONFIG TEST-DIR)
        (define c (directory->config TEST-DIR))
        (delete-directory/files test-config #:must-exist? #false)
        c)))
)
