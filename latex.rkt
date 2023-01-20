#lang racket

(provide
 beg
 thing
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
 sub
 align
 lines
 usepackage
 packages
 align*)
 

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

(define (lines  entries)
  (string-join (intersperse "\n" entries)))

(define (equation body)
  (beg "equation" body))

(define (document body)
  (beg "document" body))

(define (beg name body)
  (lines (list (thing "begin" name) (expand-body body) (thing "end" name))))

(define (m-sqrt . ...)
  (thing "sqrt" (expand-body ...)))

(define (m-pow val exponent)
  (expand-body (list val "^{" exponent "}")))

(define ^ m-pow)

(define (m-exp exponent)
  (m-pow "e" exponent))

(define (e^ . ...) (m-exp (expand-body ...)))

(define (parens . ...)
  (expand-body (list "\\left(" (expand-body ...) "\\right)")))

(define (brackets . ...)
  (expand-body (list "\\left[" (expand-body ...) "\\right]")))

(define (angs . ...)
  (expand-body (list "\\langle" (expand-body ...) "\\rangle")))

(define (sub var subscript)
  (expand-body (list var '_ "{" subscript "}")))

(define (/ a b)
  (expand-body (list "\\frac{" a "}{" b "}")))

(define (add a b)
  (expand-body (list a "+" b)))

#| What does ~a do? |#
(define (thing name arg)
  (~a "\\" name "{" arg "}"))

(define (align . entries)
  (beg "align"
    (map (lambda (x) (list '& x)) (intersperse "\\\\ \n" entries))))

(define (align* . entries)
  (beg "align*"
    (map (lambda (x) (list '& x)) (intersperse "\\\\ \n" entries))))

(define (usepackage arg) (thing "usepackage" arg))
(define (packages . ...) (lines (map usepackage ...)))
