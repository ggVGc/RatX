#lang racket
(require (for-syntax racket/base syntax/parse))

(define (plus a b)
  (+ a b))

(define (minus a b)
  (- a b))

(define (frac a b)
  (/ a b))

(define-syntax (f-old stx)
  (printf "STX: ~a~n" stx)
  (syntax-parse stx
    [(_ a b c)
     #'(a b c)]

    [(_ (a op b) xs ...) 
     #'(begin 
        (f
         (f a op b) 
         xs ...))]

    [(_ a (~datum +) b xs ...)
     #'(begin
         (f 
           (plus a b)
           xs ...))]

    [(_ a (~datum -) b xs ...)
     #'(begin
         (f 
           (minus a b)
           xs ...))]


    [(_ a (~datum /) b xs ...)
     #'(begin
         (f 
           (frac a b)
           xs ...))]

    [(_ x) #'x]
    [(_ x xs ...) #'(begin 
                     (f x) 
                     (f xs ...))]))
(define-syntax (f stx)
  (printf "STX: ~a~n" stx)
  (syntax-parse stx
    [(_) #'null]
    [(_ (~datum +) xs ...) #'(cons "+" (f xs ...))]
    [(_ (~datum -) xs ...) #'(cons "-" (f xs ...))]
    [(_ (~datum /) xs ...) #'(cons "/" (f xs ...))]

    [(_ x xs ...) 
     #'(cons x (f xs ...))]

    [(_ x) #'x]))
   
; (f "asdf" "lol" + 99 (+ 1 2 3)) 
