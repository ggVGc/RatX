#lang racket
(require "latex_base.rkt")
(require "simple_commands.rkt")

(provide (all-from-out "latex_base.rkt"))
(provide (all-from-out "simple_commands.rkt"))

(provide
 text
 pmatrix
 bmatrix
 pvec
 bvec
 pvect
 bvect
 math
 $
 beg
 beg2
 beg-opts
 m-pow
 parens
 m-sqrt
 brackets
 braces
 e^
 /
 equation
 ^
 document
 frac
 angs
 _
 lines
 packages
 align
 align*
 comma-sep
 alignpre)
 


(define (lines  entries)
  (string-join (map expand-body entries) "\n"))

(define (beg name body)
  (lines (wrapped2 (command "begin" name)  (command "end" name) body)))

(define (beg2 name args body)
  (lines (wrapped2 (command "begin" name args)  (command "end" name) body)))

(define (beg-opts name opts body)
  (lines
    (wrapped2 
      (expand-body (list (command "begin" name) "[" opts "]")) 
      (command "end" name) body)))

(define (equation . body)
  (beg "equation" (expand-body body)))

(define (document body)
  (beg "document" body))

(define (m-sqrt . ...)
  (command "sqrt" (expand-body ...)))

(define (m-pow val exponent)
  (expand-body (list val "^{" exponent "}")))

(define ^ m-pow)

(define (m-exp exponent)
  (m-pow "e" exponent))

(define (e^ . ...) (m-exp (expand-body ...)))

(define (parens . ...)
  (expand-body (command-wrapped "left(" "right)" ...)))

(define (brackets . ...)
  (expand-body (command-wrapped "left[" "right]" ...)))
(define (braces . ...)
  (expand-body (command-wrapped "left\\{" "right\\}" ...)))

(define (comma-sep . ...)
  (intersperse "," ...))

(define (angs . ...)
  (expand-body (command-wrapped "langle" "rangle" ...)))

(define (_ var subscript)
  (expand-body var '_ "{" subscript "}"))

(define (frac a b)
  (expand-body (command2 "frac" a b)))

(define / frac)

(define (add-linebreaks rows)
    (intersperse "\\\\\n" rows))

(define (align . entries)
  (beg "align" 
    (add-linebreaks (map (curry cons '&) entries))))
      
(define (alignpre pre . entries)
  (beg "align" 
    (cons (list pre "\\\\") (add-linebreaks (map (curry cons '&) entries)))))

(define (align* . entries)
  (beg "align*"
    (add-linebreaks (map (curry cons '&) entries))))

(define (packages . ...) (lines (map usepackage ...)))

(define (math . body)
  (expand-body (wrapped '$ body)))

(define $ math)

(define (matrix prefix . rows)
  (beg (string-append prefix "matrix") 
      (add-linebreaks 
        (map (curry intersperse "&") rows))))

(define (vec prefix . cols)
  (matrix prefix cols))

(define (vect prefix . cols)
  (apply (curry matrix prefix) (map list cols)))
  
(define pmatrix (curry matrix "p"))
(define bmatrix (curry matrix "b"))
(define pvec (curry vec "p"))
(define bvec (curry vec "b"))
(define pvect (curry vect "p"))
(define bvect (curry vect "b"))
(define (text . args) (command "text" (list " " args " ")))

