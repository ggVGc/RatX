#lang racket
(require "latex_base.rkt")

(provide (except-out (all-defined-out) def-simple))
(require (for-syntax syntax/parse))

(define-syntax def-simple           ; bind define2 to a
  (lambda (stx)                  ; syntax transformer that
    (syntax-parse stx            ; matches stx against
      [(_def-simple name out-name) ; this pattern, and produces
       (syntax                   ; a new syntax object
           (begin                   ; following this template
             (define (name . content) 
              (command out-name (expand-body content)))))])))

(def-simple section "section")
(def-simple dot "dot")
(def-simple hat "hat")
(def-simple bar "bar")
(def-simple vec "vec")
(def-simple cite 'cite)
(def-simple input 'input)

(define (mathcal x) (command 'mathcal x))
(define si (curry command "si"))
(define (caption . content) (command 'caption content))
(define (label name) (command "label" name))
(define (eqref name) (command "eqref" (list "eq:" name)))
(define (ref name) (command "ref" name))
(define cdot (command 'cdot))
(define Delta (command "Delta"))
(define delta (command "delta"))
(define sigma (command "sigma"))
(define (color c) (command 'color c))
(define (underline . content)
  (command 'underline content))

(define (texttt content)
  (command 'texttt content))

(define (monotext . content)
  (command 'monotext content))

(define (boxed . content)
  (command 'boxed content))

(define (usepackage . content)
  (lines (intersperse "\n" (map (curry command "usepackage") content))))

