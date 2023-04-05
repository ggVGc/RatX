#lang racket
(require (for-syntax racket/base syntax/parse))

(define (plus a b)
  (+ a b))

(define (minus a b)
  (- a b))

(define (frac a b)
  (/ a b))

(define-syntax (f stx)
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
   
; (f X)
; (f Y)
(f  1 + (* 1 2 3) + 2)
   

; (define args (syntax-e args-stx)) ; syntax->list works too
; (if (= (length args) 3)
;   (cadr args)
;   (raise-argument-error
;     'K
;     "syntax object containing a list with 3 elements"
;     args-stx))
;



; (define-syntax (m stx)
;   (printf "STX: ~a~n" stx)
;   (syntax-case stx ()
;     [(m x + y) 
;      #'"test"]
;
;     [(m (e stuff)) 
;      (printf "tail ~a~n" #'tail)
;      (syntax (e stuff))]
;
;     [(m a b) 
;      (printf "tail ~a~a~n" #'a #'b)
;      (m (syntax a))]))

; (define-syntax (f stx)
;   (printf "STX: ~a~n" stx)
;   [((~literal f)) 'ok])

; (f)
     
       
     
  ; (syntax-case stx ()
  ;   [
  ;    (_ (identifier? #'b)) (syntax "PLUS")]
  ;    
  ;   [(_ a) (syntax "a")]
  ;   ; [(_ x) #'x]
  ;   ; [(_ x xs ...) #'(begin 
  ;   ;                  (f x) 
  ;   ;                  (f xs ...))]
  ;   [_ (raise-syntax-error 'f "Bad! " stx)]))


; (f a)

; (define-syntax (swap stx)
;   (syntax-case stx ()
;     [(swap x y) #'(let ([tmp x])
;                     (set! x y)
;                     (set! y tmp))]))
;
; (define a 1)
; (define b 2)
;
; (display (list a b))
; (swap a b)
; (display (list a b))
