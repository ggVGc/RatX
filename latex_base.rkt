#lang racket

(provide 
  item-to-string
  expand-body
  intersperse
  wrapped
  wrapped2
  command-wrapped
  command
  command2)

(define (intersperse separator lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (cons (car lst) (cons separator (intersperse separator (cdr lst))))))

(define (item-to-string val)
  (cond
    [(string? val) val]
    [(number? val) (number->string val)]
    [(symbol? val) (symbol->string val)]))

(define (expand-body . arg)
  (if (list? arg)
    (string-join (map item-to-string (flatten arg)) "")
    (item-to-string arg)))


#| What does ~a do? |#
(define (command name . args)
  (if (null? args)
    (~a "\\" name " ") 
    (apply (curry command2 name) args)))

(define (command2 name . args)
    (~a "\\" name (string-join (map 
                                (lambda (x) (string-join (list "{" (expand-body x) "}") ""))
                                args) "")))

(define (wrapped2 delim_a delim_b body)
   (list delim_a body delim_b))

(define (wrapped delimiter body)
   (wrapped2 delimiter delimiter body))

(define (command-wrapped a b . body)
   (list (command a) " " body " " (command b)))
