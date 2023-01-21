#lang racket
(provide (all-defined-out))
(require "latex_base.rkt")
(require (for-syntax syntax/parse))

(define-syntax def-simple           ; bind define2 to a
  (lambda (stx)                  ; syntax transformer that
    (syntax-parse stx            ; matches stx against
      [(_def-simple name out-name) ; this pattern, and produces
       (let (
              [asdf 123]) 
         (syntax                   ; a new syntax object
           (begin                   ; following this template
             (define (name . content) (command out-name (expand-body content))))))])))
 
(def-simple section "section")
(def-simple usepackage "usepackage")
