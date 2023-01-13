#lang racket

(provide
 expand-body
 m-pow
 parens
 m-sqrt
 brackets
 e^
 / equation
 ^
 document
 add
 angs
 )

(define (item-to-string val)
  (cond
    [(string? val) val]
    [(number? val) (number->string val)]
    [(symbol? val) (symbol->string val)]))

(define (expand-body arg)
  (cond
    [(list? arg) (string-join (map item-to-string (flatten arg)))]
    [(string? arg) arg]
    [else (error (~a "Unsupported body argument: " arg))]))


(define (intersperse separator lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (cons (car lst) (cons separator (intersperse separator (cdr lst))))))

(define (lines . ...)
  (string-join (intersperse "\n" ...)))

(define (equation body)
  (beg "equation" body))

(define (document body)
  (beg "document" body))

(define (beg name body)
  (lines (thing "begin" name) (expand-body body) (thing "end" name)))

(define (m-sqrt . ...)
  (thing "sqrt" (expand-body ...)))

(define (m-pow val exponent)
  (expand-body (list val "^{" exponent "}")))

(define ^ m-pow)

(define (m-exp exponent)
  (m-pow "e" exponent))

(define (e^ . ...) (m-exp (expand-body ...)))

(define (parens . ...)
  (list "\\left(" (expand-body ...) "\\right)"))

(define (brackets . ...)
  (list "\\left[" (expand-body ...) "\\right]"))

(define (angs . ... )
  (list "\\langle" (expand-body ...) "\\rangle"))

(define (/ a b)
  (list "\\frac{" a "}{" b "}"))

(define (add a b)
  (list a "+" b))

#| What does ~a do? |#
(define (thing name arg)
  (~a "\\" name "{" arg "}"))
