#lang racket
(require "latex_base.rkt")
(require "simple_commands.rkt")

(provide (all-from-out "latex_base.rkt"))
(provide (all-from-out "simple_commands.rkt"))
(provide
 math
 beg
 m-pow
 parens
 m-sqrt
 brackets
 e^
 /
 +
 equation
 ^
 document
 add
 frac
 angs
 _
 lines
 packages
 align
 align*)
 


(define (lines  entries)
  (string-join (intersperse "\n" entries) ""))

(define (beg name body)
  (lines (wrapped2 (command "begin" name)  (command "end" name) body)))

(define (equation . body)
  (beg "equation" (expand-body body)))

(define (document body)
  (beg "document" body))


(define (m-sqrt . ...)
  (command "sqrt" (expand-body ...)))

(define (m-pow val exponent)
  (expand-body (list val "^{" exponent "}")))

(define ^ m-pow)

(define (m-exp exponent)
  (m-pow "e" exponent))

(define (e^ . ...) (m-exp (expand-body ...)))

(define (parens . ...)
  (expand-body (command-wrapped "left(" "right)" ...)))

(define (brackets . ...)
  (expand-body (command-wrapped "left[" "right]" ...)))

(define (angs . ...)
  (expand-body (command-wrapped "langle" "rangle" ...)))

(define (_ var subscript)
  (expand-body var '_ "{" subscript "}"))

(define (frac a b)
  (expand-body (command2 "frac" a b)))

(define / frac)

(define (add a b)
  (expand-body a "+" b))

(define + add)

(define (align . entries)
  (beg "align"
    (intersperse "\\\\ \n" (map (curry cons '&) entries))))
      

(define (align* . entries)
  (beg "align*"
    (intersperse "\\\\ \n" (map (curry cons '&) entries))))

(define (packages . ...) (lines (map usepackage ...)))


(define (math body)
  (wrapped '& body))
