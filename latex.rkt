#lang racket
(require "latex_base.rkt")
(require "simple_commands.rkt")

(provide (all-from-out "latex_base.rkt"))
(provide (all-from-out "simple_commands.rkt"))

(provide
 text
 pmatrix
 bmatrix
 vmatrix
 pvec
 bvec
 pvect
 bvect
 math
 math2
 $
 beg
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
 newlines
 packages
 cases
 align
 align-on
 align-on*
 align*
 separated
 comma-sep
 alignpre
 underbrace
 overbrace
 italic
 bold
 bibliography
 alpha-subsections
 doc-begin
 doc-end
 make-title
 image
 imagew
 smallimage
 side-by-side
 verbatim
 section-start
 subsection
 subsection*
 subsubsection
 subsubsection*
 figure
 figure-here
 itemize
 item
 items)

(define make-title (command 'maketitle))

(define (doc-begin [title ""] [author ""] [date ""])
  (lines
   (command 'title title)
   (command 'author author)
   (command 'date date)
   (command 'begin 'document)))

(define doc-end (command 'end 'document))

(define (newlines . entries)
  (intersperse "\\\\\n" entries))

(define (opt-expand opt)
  (if (null? opt) null (list "[" opt "]")))

(define (arg-expand arg)
  (if (null? arg) null (list "{" arg "}")))

(define (beg #:opt [opt null] #:arg [arg null] name body)
  (lines
   (list
    (command "begin" name)
    (opt-expand opt)
    (arg-expand arg))
   body
   (command "end" name)))

(define (equation . body)
  (beg "equation" body))

(define (document body)
  (beg "document" body))

(define (m-pow val . exponent)
  (if
   (null? exponent)
   (list "^{" val "}")
   (list val "^{" exponent "}")))

(define (^ . args)
  (if
   (null? args)
   '^
   (apply m-pow args)))

(define (m-exp exponent)
  (m-pow "e" exponent))

(define (e^ . ...) (m-exp ...))

(define (leftright left right . body)
  (list
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
  (list
   (command "langle")
   body
   (command "rangle")))

(define (separated separator . ...)
  (intersperse separator ...))

(define (comma-sep . ...)
  (apply separated ",\\," ...))

(define (underscore var . subscript)
  (if
   (null? subscript)
   (list '_ "{" var "}")
   (list var '_ "{" subscript "}")))

(define (_ . args)
  (if
   (null? args)
   '_
   (apply underscore args)))

(define (align . entries)
  (beg "align"
       (apply newlines (map (curry cons '&) entries))))

(define (cases . entries)
  (beg "cases"
       (apply newlines entries)))

(define (align-on aligner . lines)
  (define align-str (item-to-string aligner))

  (beg "align"
       (string-replace
        (expand-body (apply newlines lines))
        align-str
        (string-append "&" align-str))))

(define (align-on* aligner . lines)
  (define align-str (item-to-string aligner))

  (beg "align*"
       (string-replace
        (expand-body (apply newlines lines))
        align-str
        (string-append "&" align-str))))


(define (alignpre pre fst . entries)
  (beg "align"
       (cons
        (list pre '& fst "\\\\") 
        (apply newlines 
               (map (curry cons '&) entries)))))

(define (align* . entries)
  (beg "align*"
       (apply newlines (map (curry cons '&) entries))))

(define (packages . ...) (lines (map usepackage ...)))

(define (math . body)
  (wrapped "$" body))

(define (math2 . body)
  (list 
   (command "[") 
   body
   (command "]"))) 

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
(define vmatrix (curry matrix "v"))
(define pvec (curry vec "p"))
(define bvec (curry vec "b"))
(define pvect (curry vect "p"))
(define bvect (curry vect "b"))

#| (define (text . args) (command "text" (list " " args " "))) |#
(define (text . content)
  (command 'text content))


(define (underbrace tag . content)
  (_ (command "underbrace" content) tag))

(define (overbrace tag . content)
  (m-pow (command "overbrace" content) tag))


(define (bibliography . entries)
  (beg #:arg "3" "thebibliography" 
       (map
        (lambda (x)
          (list
           (command "bibitem" (first x))
           (rest x)))
        entries)))

(define (alpha-subsections)
  (command "renewcommand"
           (command "thesubsection")
           (list (command "thesection") "." (command "alph" "subsection"))))

; If caption is given, a label will be automatically set
; based on the path.
(define (image path #:width [width 1]. caption-content)
  (list
   (command (list "includegraphics[width=" width "\\linewidth]") path)
   (if (not (null? caption-content))
       (list
        (caption caption-content)
        (label path))
       null)
   ))

(define (imagew width path)
  (command (list "includegraphics[width=" width "\\linewidth]") path))

(define (smallimage path)
  (command "includegraphics[width=0.5\\linewidth]" path))

(define (side-by-side a b)
  (lines
   (sbs-page a)
   (sbs-page b)))


(define (sbs-page page)
  (beg
   #:opt "t" 
   #:arg (list 0.48 (command 'textwidth))
   "minipage"
   (list 
    (apply lines 
           (list 
            (command 'centering)
            page)))))     

(define verbatim (curry beg 'verbatim))

(define (figure #:opt [opt "htb"] . body)
  (beg #:opt opt 'figure (list
                          (command 'centering)
                          body)))

(define (figure-here . body) 
  (apply figure #:opt "H" body))

(define (section-start n)
  (command "setcounter" "section" n))

(define (subsection . ...)
  (command 'subsection ...)
  #| (lines "" (command 'subsection ...) "")) |#)

(define (subsubsection . ...)
  (command 'subsubsection ...)
  #| (lines "" (command 'subsubsection ...) "")) |#)

(define (subsubsection* . ...)
  (command 'subsubsection* ...)
  #| (lines "" (command 'subsubsection* ...) "")) |#)

(define (subsection* . ...)
  (command 'subsection* ...)
  #| (lines "" (command 'subsection* ...) "")) |#)


(define (itemize . body)
  (beg "itemize" body))

(define item (curry command 'item))

(define (items . rows)
  (define (empty-string? row)
    (and (string? row) (equal? (string-trim row) "")))

  (beg 'itemize
       (apply lines (for/list ([row rows])
                      (if (empty-string? row)
                          null
                          (command 'item row)
                          )
                      ))))


(module+ test
  (require rackunit)

  (check-equal?
   (expand-body (parens "body"))
   "\\left(body\\right)")

  (check-equal?
   (expand-body (angs "body"))
   "\\langle body\\rangle ")

  (check-equal?
   (expand-body (math "body"))
   "$body$") 

  (check-equal?
   (expand-body (opt-expand "a"))
   "[a]")) 
