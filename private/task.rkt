#lang racket/base

(provide
  resume-task
  init-task
)

(require
  )

;; =============================================================================

(define (task-print t port write?)
  (if write?
    (fprintf port
             "Task ~a~n- targets : ~a~n- config : ~a~n"
             (gtp-measure-task-uid t)
             (gtp-measure-task-targets t)
             (gtp-measure-task-config t))
    (fprintf port "#<task:~a>" (gtp-measure-task-uid t))))

(struct gtp-measure-task [uid targets config]
  #:methods gen:custom-write [(define write-proc task-print)])

(define (resume-task targets)
  (void))

(define (init-task targets config)
  (void))

;; =============================================================================

(module+ test
  (require rackunit)

)
