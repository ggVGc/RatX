#lang racket
(provide
 squared
 frac
 hbar2
 paragraph
 ~
 malign)

(require
  syntax/parse/define
  (only-in
   "latex.rkt"
   ^
   figure))

(require
  (only-in
   "latex.rkt"
   align)
  (only-in
   "math.rkt"
   fraction
   hbar))


(define paragraph list)
(define ~ paragraph)


(require (for-syntax racket/base))

(define squared (^ 2))
(define hbar2 (list hbar squared))

(define-syntax-parse-rule (malign (entries ...) ...)
  (align
   (list entries ...) ...))

(define-syntax (frac stx)
  (syntax-case stx ()
    [(_ a b)
     #'(
        fraction
        (list-wrap a)
        (list-wrap b))]))

(define-syntax (list-wrap stx)
  (syntax-case stx ()
    [(_ (a ...)) #'(list a ...)]
    [(_ a) #'(list a)]))
