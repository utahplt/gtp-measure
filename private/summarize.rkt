#lang racket/base

;; Stage 3: Summarize results

;; ... finished job, name target, when started, what results, any sub-jobs
;; be generic?

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

