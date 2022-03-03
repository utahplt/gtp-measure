#lang racket/base

;; Defines a #lang for typed/untyped output

;; TODO parse and validate the data

(provide
  (rename-out [-#%module-begin #%module-begin]))

(require
  (for-syntax
    racket/base
    syntax/parse))

;; -----------------------------------------------------------------------------

(define-syntax (-#%module-begin stx)
  (syntax-parse stx
   [(_ . data*)
    (define (make-error expected val pos [ctx #f])
      (format "elem ~a : expected ~a got ~s~a"
              pos expected val (if ctx (format " in ~e" ctx) "")))
    (define (parse-value v pos)
      (if (not (and (pair? v) (pair? (cdr v)) (null? (cddr v))))
        (make-error "(list/c any/c any/c)" v pos)
        (let-values (((fst snd) (values (car v) (cadr v))))
          (cond
            [(not (string? fst))
             (make-error "string?" fst pos v)]
            [(not (and (list? snd) (andmap string? snd)))
             (make-error "(listof string?)" snd pos v)]
            [else
             (define-values [min-t max-t total-t]
               (for/fold ([min-t #f]
                          [max-t #f]
                          [total-t 0])
                         ((str (in-list snd)))
                 (define m (regexp-match #rx"cpu time: ([0-9]+) " str))
                 (if m
                   (let ([n (string->number (cadr m))])
                     (values (if min-t (min min-t n) n)
                             (if max-t (max max-t n) n)
                             (+ total-t n)))
                   (values min-t max-t total-t))))
             (list (length snd) min-t max-t total-t)]))))
    (define quick-stats
      (for/fold ([acc (hasheq 'num-configs 0
                              'num-times 0
                              'min-cpu-time #f
                              'max-cpu-time #f
                              'total-cpu-time 0
                              'error* '())])
                ([val (in-list (syntax->datum #'data*))]
                 [pos (in-naturals 0)])
        (define cfg-info (parse-value val pos))
        (if (string? cfg-info)
          (hash-update acc 'error* (lambda (e*) (cons cfg-info e*)))
          (let* ([new-num-times (car cfg-info)]
                 [new-min-t (cadr cfg-info)]
                 [new-max-t (caddr cfg-info)]
                 [new-total-t (cadddr cfg-info)]
                 [acc (hash-update acc 'num-configs (lambda (n) (+ n 1)))]
                 [acc (hash-update acc 'num-times (lambda (n) (+ n new-num-times)))]
                 [acc (hash-update acc 'min-cpu-time (lambda (v) (if v (if new-min-t (min v new-min-t) v) new-min-t)))]
                 [acc (hash-update acc 'max-cpu-time (lambda (v) (if v (if new-max-t (max v new-max-t) v) new-max-t)))]
                 [acc (hash-update acc 'total-cpu-time (lambda (n) (+ n new-total-t)))])
            acc))))
    (with-syntax ([summary-str
                    (let ([err* (hash-ref quick-stats 'error*)])
                      (if (null? err*)
                        (string-append
                          "dataset info:\n"
                          (format "- num configs: ~a~n" (hash-ref quick-stats 'num-configs))
                          (format "- num timings: ~a~n" (hash-ref quick-stats 'num-times))
                          (format "- min time: ~a ms~n" (or (hash-ref quick-stats 'min-cpu-time) "???"))
                          (format "- max time: ~a ms~n" (or (hash-ref quick-stats 'max-cpu-time) "???"))
                          (format "- total time: ~s ms" (or (hash-ref quick-stats 'total-cpu-time) "???")))
                        (apply string-append
                               "dataset errors:"
                               (for/fold ((acc '()))
                                         ((s (in-list err*)))
                                 (cons (string-append "\n- " s) acc)))))])
      #`(#%module-begin
         (define gtp-data 'data*)
         (provide gtp-data)
         (module+ main
           (displayln summary-str))))]))

(module* reader syntax/module-reader
  gtp-measure/output/deep-shallow-untyped
  #:read s:read
  #:read-syntax s:read-syntax
  (require (prefix-in s: scribble/reader)))

