#lang racket
(require "latex.rkt")

(provide dd)

(define (dd variable . degree)
  (if (null? degree)
      (expand-body (/ "d" (list "d" variable)))
      (expand-body (/ (m-pow "d" degree) (list "d" (m-pow variable degree))))))

