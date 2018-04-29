#lang racket/base

;; Measure the performance of a "task"

(provide
  measure)

(require
  gtp-measure/private/util
  gtp-measure/private/task)

;; =============================================================================

(define (measure task)
  (define st* (in-subtasks task))
  (define num-subtasks (length st*))
  (define fmt (make-progress-counter num-subtasks "subtask"))
  (log-gtp-measure-info "begin measuring ~a (~a subtasks)" task num-subtasks)
  (for ((st (in-list st*))
        (i (in-naturals 1)))
    (log-gtp-measure-info "~a begin measuring ~a" (fmt i) st)
    (subtask-run! st))
  (log-gtp-measure-info "end measuring ~a" task)
  (void))

;; =============================================================================

(module+ test
  (require rackunit)

)

