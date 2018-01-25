#lang racket/base

;; Stage 1: Configure a job

;; ... given some targets, get ready to start measuring performance
;; ... write a file of things to do
;; ... be generic?

(provide
  key:entry-point
  key:bin
  key:iterations
  key:sample-size
  key:num-samples
  key:jit-warmup)

(define key:entry-point 'entry-point)
(define key:bin 'bin)
(define key:iterations 'iterations)
(define key:sample-size 'sample-size)
(define key:num-samples 'num-samples)
(define key:jit-warmup 'jit-warmup-iterations)
