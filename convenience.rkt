#lang racket
(provide 
  squared
  frac
  hbar2
  paragraph
  ~)

(require 
  (only-in 
    "latex.rkt" 
    ^ 
    figure))

(require 
  (only-in 
    "math.rkt"
    fraction
    hbar))


(define paragraph list)
(define ~ paragraph)


(require (for-syntax racket/base))

(define squared (^ 2))
(define hbar2 (list hbar squared))


(define-syntax (frac stx)
  (syntax-case stx ()
    [(_ a b) 
     #'(
        fraction 
        (list-wrap a)
        (list-wrap b))]))

(define-syntax (list-wrap stx)
  (syntax-case stx ()
    [(_ (a)) #'(list a)]
    [(_ (a b)) #'(list a b)]
    [(_ (a b c)) #'(list a b c)]
    [(_ (a b c d)) #'(list a b c d)]
    [(_ (a b c d e)) #'(list a b c d e)]
    [(_ (a b c d e f)) #'(list a b c d e f)]
    [(_ (a b c d e f a1)) #'(list a b c d e f a1)]
    [(_ (a b c d e f a1 a2)) #'(list a b c d e f a1 a2)]
    [(_ (a b c d e f a1 a2 a3)) #'(list a b c d e f a1 a2 a3)]
    [(_ (a b c d e f a1 a2 a3 a4)) #'(list a b c d e f a1 a2 a3 a4)]
    [(_ a) #'(list a)]))
