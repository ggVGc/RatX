#lang racket/base
(require "latex.rkt")

(provide (all-defined-out))

(define (dd variable . degree)
  (if (null? degree)
      (/ "d" (list "d" variable))
      (/ (m-pow "d" degree) (list "d" (m-pow variable degree)))))


(define (ln content)
  (command "ln" content))

(define (sum start end . body)
  (list
    (^ (_ (command "sum") start) end)
    body))

(define (abs v) (command 'abs v))
(define pi (command 'pi))
(define epsilon (command 'epsilon))
(define mu (command 'mu))
(define inf (command 'infty))
(define theta (command 'theta))
(define phi (command 'phi))
(define psi (command 'psi))
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

