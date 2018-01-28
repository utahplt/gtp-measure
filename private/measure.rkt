#lang racket/base

;; Stage 2: Measure performance

;; ... given some description of a benchmark, start measuring performance
;; ... parse a file of things to do, checkpoint the things done
;; be generic?

(provide
  measure)

(require
  gtp-measure/private/task)

;; =============================================================================

(define (measure task)
  (void))

;; =============================================================================

(module+ test
  (require rackunit)

)

