#lang racket/base

;; Stage 2: Measure performance

;; ... given some description of a benchmark, start measuring performance
;; ... parse a file of things to do, checkpoint the things done
;; be generic?

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
  (void))

;; =============================================================================

(module+ test
  (require rackunit)

)

