#lang racket/base
(require "latex_base.rkt")

(provide (all-defined-out))

(define (ket . x) (command "ket" x))
(define (bra . x) (command "bra" x))
(define (braket a b) (command2 "braket" a b))

