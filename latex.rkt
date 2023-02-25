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
 math2
 $
 beg
 beg2
 beg-opts
 m-pow
 parens
 brackets
 braces
 e^
 equation
 ^
 document
 angs
 _
 lines
 newlines
 packages
 align
 align*
 comma-sep
 alignpre
 underbrace
 overbrace
 italic
 bold
 bibliography)
 


(define (lines . entries)
  (string-join (map expand-body entries) "\n"))

(define (newlines . entries)
  (intersperse (command "\\") entries))

(define (beg name body)
  (lines 
    (list
      (command "begin" name) 
      "\n"
      body
      "\n"
      (command "end" name))))

(define (beg2 name args body)
  (lines
    (command "begin" name args)
    body
    (command "end" name)))

(define (beg-opts name opts body)
  (lines
      (expand-body (list (command "begin" name) "[" opts "]")) 
      body
      (command "end" name))) 

(define (equation . body)
  (beg "equation" body))

(define (document body)
  (beg "document" body))

(define (m-pow val exponent)
  (expand-body (list val "^{" exponent "}")))

(define ^ m-pow)

(define (m-exp exponent)
  (m-pow "e" exponent))

(define (e^ . ...) (m-exp (expand-body ...)))

(define (leftright left right . body)
  (expand-body 
    (list "\\left" left)
    body
    (list "\\right" right))) 
  
(define (parens . ...)
  (leftright "(" ")" ...))

(define (brackets . ...)
  (leftright "[" "]" ...))

(define (braces . ...)
  (leftright "{" "}" ...))

(define (angs . body)
  (expand-body 
                        (command "langle")
                        body
                        (command "rangle")))

(define (comma-sep . ...)
  (intersperse "," ...))

(define (_ var . subscript)
  (if
    (null? subscript)
    (list '_ "{" subscript "}")
    (list var '_ "{" subscript "}")))


(define (align . entries)
  (beg "align" 
    (apply newlines (map (curry cons '&) entries))))
      
(define (alignpre pre . entries)
  (beg "align" 
    (cons (list pre "\\\\") (apply newlines (map (curry cons '&) entries)))))

(define (align* . entries)
  (beg "align*"
    (apply newlines (map (curry cons '&) entries))))

(define (packages . ...) (lines (map usepackage ...)))

(define (math . body)
  (expand-body (wrapped '$ body)))

(define (math2 . body)
  (expand-body 
    (wrapped2 
      (command "[") 
      (command "]") 
      body)))

(define $ math)

(define (matrix prefix . rows)
  (beg (string-append prefix "matrix") 
      (apply newlines 
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
(define (underbrace tag . content)
  (_ (command "underbrace" content) tag))

(define (overbrace tag . content)
  (^ (command "overbrace" content) tag))


(define (bibliography . entries)
  (beg2 "thebibliography" "3" 
    (map
      (lambda (x)
        (list
          (command "bibitem" (first x))
          (rest x)))
      entries)))

(define (italic x)
  (command "textit" x))

(define (bold x)
  (command "textbf" x))

(module+ test
  (require rackunit)

  (check-equal?
    (parens "body")
    "\\left(body\\right)")

  (check-equal?
    (angs "body")
    "\\langle body\\rangle ")

  (check-equal?
    (math "body")
    "$body$"))
