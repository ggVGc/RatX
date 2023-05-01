#lang racket/base
(require racket/match)

(require "latex.rkt")

(provide (all-defined-out))

(define > ">")
(define < "<")

(define (dd variable . degree)
  (if (null? degree)
      (fraction "d" (list "d" variable))
      (fraction (m-pow "d" degree) (list "d" (m-pow variable degree)))))


(define (ln . content)
  (command "ln" content))

(define (sum start end . body)
  (list
    (^ (_ (command "sum") start) end)
    body))

(define (abs . v) (list "\\left|" v "\\right|"))

(define (prod start end . body)
 (list
   (^ (_ (command "prod") start) end)
   body))

(define (sqrt . ...)
  (command "sqrt" (expand-body ...)))

(define (fraction a b)
  (command "frac" a b))

(define (part-deriv a b)
  (fraction
    (list partial a)
    (list partial b)))

(define partial (command 'partial))
(define pi (command 'pi))
(define inf (command 'infty))
(define implies (command 'implies))
(define pm (command 'pm))
(define (cos . args) (command 'cos args))
(define (sin . args) (command 'sin args))
(define (tan . args) (command 'tan args))
(define (arcsin . args) (command 'arcsin args))
(define (arccos . args) (command 'arccos args))
(define approx (command 'approx))
(define hbar (command 'hbar))
(define + '+)
(define = '=)
(define - '-)
(define prim "'")

(define (SI value . units)
  (define (build-units x)
    (map (lambda (u) (list "\\" u)) x))

  (command "SI"
    value
    (match units
      [(list entry) #:when (string? entry) entry]
      [(list entry) #:when (list? entry) 
          (build-units entry)]          
      [_ 
        (build-units units)])))

(define (SI-err val err . units)
  (apply SI
    (list val pm err)
    units))

(module+ test
  (require rackunit)

  (check-equal?
    (SI 123 "hej" 'lol)
    "\\SI{123}{\\hej \\lol }")

  (check-equal?
    (SI 123 "test")
    "\\SI{123}{test}")

  (check-equal?
    (SI 123 'test)
    "\\SI{123}{\\test }"))
