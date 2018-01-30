#lang racket/base

;; Measure the performance of a "task"

(provide
  measure)

(require
  gtp-measure/private/util
  gtp-measure/private/task)

;; =============================================================================

(define (measure task)
  (log-gtp-measure-info "begin measuring ~a" task)
  (for ((st (in-subtasks task)))
    (log-gtp-measure-info "begin measuring ~a" st)
    (subtask-run! st))
  (log-gtp-measure-info "end measuring ~a" task)
  (void))

;; =============================================================================

(module+ test
  (require rackunit)

)

