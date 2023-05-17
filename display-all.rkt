#lang racket
(require syntax/wrap-modbeg)

(define (item-to-string val)
  (cond
    [(procedure? val) (item-to-string (val))]
    [(string? val) val]
    [(number? val) (number->string val)]
    [(symbol? val) (symbol->string val)]))
  

; https://stackoverflow.com/questions/45786187/how-to-maintain-define-functionality-with-custom-module-begin

(provide
  (except-out (all-from-out racket) #%module-begin)
  (rename-out [module-begin #%module-begin]))

(define-syntax module-begin (make-wrapping-module-begin #'wrap-expression))
(define-syntax (wrap-expression stx)
  (syntax-case stx ()
    [(_ expr) 
     #'(printf "~a~n" 
         (if (list? expr)
             (string-join (map item-to-string (flatten expr)) "")
             (item-to-string expr)))]))
