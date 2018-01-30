#lang racket/base

;; Summarize the results of a task

(provide
  summarize)

(require
  gtp-measure/private/util
  gtp-measure/private/task)

;; =============================================================================

(define (summarize task)
  (printf "Finished measuring task~n~s~n" task)
  (printf "Results in '*.out' files in directory ~a~n" (task->directory task))
  (void))

;; =============================================================================

(module+ test
  (require rackunit)

)

