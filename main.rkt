#lang racket

(define (intersperse separator lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (cons (car lst)
            (cons separator
                  (intersperse separator (cdr lst))))))

#| What does ~a do? |#
(define (thing name arg)
  (~a "\\" name "{" arg "}"))

(define (num-to-string val)
  (if (string? val) val (number->string val)))

(define (expand-body arg)
  (cond
    [(list? arg)
     (string-join (map num-to-string (flatten arg)))]
    [(string? arg) arg]
    [else (error (~a "Unsupported body argument: " arg))]))

(define (lines lst)
  (string-join (intersperse "\n" lst)))

(define (beg name body)
  (lines (list (thing "begin" name)
               (expand-body body)
               (thing "end" name))))

(define (equation body)
  (beg "equation" body))

(define (document body)
  (beg "document" body))

(define (m-sqrt body)
  (thing "sqrt" body))

(define (m-pow val exponent)
  (list val "^{" exponent "}"))

(define ^ m-pow)

(define (m-exp exponent)
  (m-pow "e" exponent))

(define e^ m-exp)

(define (parens body)
  (list "\\left(" body "\\right)"))

(define (angs body)
  (list "\\langle" body "\\rangle"))

(define (/ a b)
  (list "\\frac{" a "}{" b "}"))

(define (add a b)
  (list a "+" b))

(define doc-content
  (list (equation
         (list (e^ (angs (add 3 6))) (/ 123 55) (e^ 11)))))

(display (document doc-content))
