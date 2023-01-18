#lang racket
(require "latex.rkt")
#| (require "math.rkt") |#

#| (define doc-content |#
#|   (list (equation |#
#|          (list (e^ (angs (add 3 6))) (/ 123 55) (e^ 11))))) |#

(define (anki-surround . ...)
  (string-join (list "\\(" (expand-body ...) "\\)")))

#| (dd 't 2) 'f '+ 'y (dd 't) 'f '+ '\\omega_0 'f '=0 |#
#| (list 'A (e^ (list '\\alpha 't))) |#

(define over-damped
  (let ([cont (parens (m-sqrt (m-pow '\\omega_0 2) '- (/ (m-pow '\\gamma 2) 4)) 't)])
    (list 
      (brackets 'A '\\cos cont '+ 'B '\\sin cont)
      (e^ '- (list '\\gamma 't) '/ 2))))


(define under-damped
   (let [(cont
           (list 
             't (m-sqrt 
                  (/ (^ '\\gamma 2) 4) 
                  '- 
                  '\\omega_0^2)))]
     (list 
       (brackets 'A (e^ cont) '+ 'B (e^ '- cont)) 
       (e^ '- (list '\\gamma 't) '/ 2))))

#| (define critical-damping (list (parens '(A + Bt)) (e^ '(- \\gamma t / 2)))) |#

#| (define cur-equation critical-damping) |#
(define cur-equation under-damped)

(display (anki-surround cur-equation))
