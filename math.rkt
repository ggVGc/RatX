#lang racket
(require "latex.rkt")

(provide dd ln)

(define (dd variable . degree)
  (if (null? degree)
      (/ "d" (list "d" variable))
      (/ (m-pow "d" degree) (list "d" (m-pow variable degree)))))


(define (ln content)
  (command "ln" content))
