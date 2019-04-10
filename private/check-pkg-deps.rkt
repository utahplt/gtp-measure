#lang racket/base

(require racket/contract)
(provide
  (contract-out
    [check-pkg-deps
      (->* [directory-exists?] [#:auto? any/c] void?)]
    ;; Given the `bin/` folder of a Racket install, check whether
    ;;  the install has some necessary packages.
    ;; If the packages are missing and #:auto? is non-#false, then
    ;;  use `bin/raco` to install the packages.
    ))

(require
  (only-in gtp-measure/private/util
    log-gtp-measure-error)
  (only-in gtp-measure/private/task
    bin->rackets)
  (only-in racket/string
    string-join)
  (only-in racket/port
    open-output-nowhere)
  (only-in racket/system
    system*))

;; =============================================================================

(define PKG* '("require-typed-check"))

(define (check-pkg-deps bin #:auto? [auto? #true])
  (define-values [raco-bin racket-bin] (bin->rackets bin))
  (define need-to-install*
    (for/list ((pkg (in-list PKG*))
               #:unless (pkg-installed? pkg racket-bin))
      pkg))
  (unless (null? need-to-install*)
    (define formatted-pkgs (string-join need-to-install* ", "))
    (log-gtp-measure-error "missing likely-necessary package~a: ~a"
                           (if (null? (cdr need-to-install*)) "" "s")
                           formatted-pkgs)
    (cond
      [auto?
        (log-gtp-measure-error "installing ...")
        (unless (apply system* raco-bin "pkg" "install" "--auto" need-to-install*)
          (raise-user-error 'gtp-measure "failed to install packages: ~a" formatted-pkgs))]
      [else
       (log-gtp-measure-error "continuing without installing the packages")])))

(define (pkg-installed? pkg-name racket-bin)
  (define dead-end (open-output-nowhere))
  (begin0
    (parameterize ((current-output-port dead-end)
                   (current-error-port dead-end))
      (system* racket-bin "--lib" pkg-name))
    (close-output-port dead-end)))

;; 2019-04-09 : I tested this locally with one racket that already had the
;;  package and one racket that needed to install it. Looked good. Not sure
;;  how to turn into a nice unit test.
;; (module+ main (check-pkg-deps "..../racket/6.7/racket/bin/"))
