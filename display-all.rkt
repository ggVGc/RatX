#lang racket
(require syntax/wrap-modbeg)

; https://stackoverflow.com/questions/45786187/how-to-maintain-define-functionality-with-custom-module-begin

(provide
  (except-out (all-from-out racket) #%module-begin)
  (rename-out [module-begin #%module-begin]))

(define-syntax module-begin (make-wrapping-module-begin #'wrap-expression))
(define-syntax (wrap-expression stx)
  (syntax-case stx ()
    [(_ expr) #'(printf "~a~n" expr)]))
