#lang racket/base
(require "latex.rkt")

(provide (all-defined-out))

(define (dd variable . degree)
  (if (null? degree)
      (/ "d" (list "d" variable))
      (/ (m-pow "d" degree) (list "d" (m-pow variable degree)))))


(define (ln . content)
  (command "ln" content))

(define (sum start end . body)
  (list
    (^ (_ (command "sum") start) end)
    body))

(define (abs v) (list "|" v "|"))

(define (prod start end . body)
 (list
   (^ (_ (command "prod") start) end)
   body))

(define (frac a b)
  (command "frac" a b))

(define / frac)

(define pi (command 'pi))
(define inf (command 'infty))
(define implies (command 'implies))
(define pm (command 'pm))
(define (cos . args) (command 'cos args))
(define (sin . args) (command 'sin args))
(define (arcsin . args) (command 'arcsin args))
(define (arccos . args) (command 'arccos args))
(define approx (command 'approx))
(define + '+)
(define = '=)
(define - '-)
(define prim "'")
