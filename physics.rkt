#lang racket
(require "latex_base.rkt")

(provide (all-defined-out))

(define (ket . x) (command "ket" x))
(define (bra . x) (command "bra" x))
(define braket (curry command "braket"))
(define expval (curry command "expval"))

