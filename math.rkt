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
  (command "sqrt" ...))

(define (fraction a b)
  (command "frac" a b))

(define (deriv a b)
  (fraction
   (list 'd  a)
   (list 'd b)))

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
(define degree (^ (command 'circ)))
(define nabla (command 'nabla))

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

(define (integral-indef integrand . body)
  (list 
   (command 'int)
   body
   "\\,"
   "d" integrand))

(define (integral-multi integrand start stop . body)
  (list 
   (command 'int)
   body
   "\\,"
   "d" integrand _ start
   (command 'ldots)
   "d" integrand _ stop))

(define (integral a b integrand . body)
  (list 
   (command 'int) _ a ^  b
   body
   "\\,"
   "d" integrand))

(module+ test
  (require rackunit)

  (check-equal?
   (expand-body (SI 123 "hej" 'lol))
   "\\SI{123}{\\hej\\lol}")

  (check-equal?
   (expand-body (SI 123 "test"))
   "\\SI{123}{test}")

  (check-equal?
   (expand-body (SI 123 'test))
   "\\SI{123}{\\test}") 

  (check-equal?
   (expand-body (sqrt 123 "test"))
   "\\sqrt{123test}"))
