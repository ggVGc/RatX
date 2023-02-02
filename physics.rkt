#lang racket
(require "latex_base.rkt")

(provide (all-defined-out))

(define (ket . x) (command "ket" x))
(define (bra . x) (command "bra" x))
(define braket (curry command2 "braket"))
(define expval (curry command2 "expval"))

