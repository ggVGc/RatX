#lang racket

(provide 
  item-to-string
  expand-body
  intersperse
  wrapped
  wrapped2
  command)

(define (intersperse separator lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (cons (car lst) (cons separator (intersperse separator (cdr lst))))))

(define (item-to-string val)
  (cond
    [(procedure? val) (item-to-string (val))]
    [(string? val) val]
    [(number? val) (number->string val)]
    [(symbol? val) (symbol->string val)]))

(define (expand-body . arg)
  (if (list? arg)
    (string-join (map item-to-string (flatten arg)) "")
    (item-to-string arg)))


(define (command name . args)
  (expand-body
    (list "\\" name
      (if (null? args)
        " "
        (map 
          (lambda (x) (list "{" x "}"))
          args)))))

(define (wrapped a . body)
  (list a body a))

(define (wrapped2 a b . body)
  (list a body b))

(module+ test
  (require rackunit)

  (check-equal?
    (command "cmd")
    "\\cmd ")

  (check-equal?
    (command "cmd" "arg")
    "\\cmd{arg}")

  (check-equal?
    (command "cmd" 12 'b)
    "\\cmd{12}{b}")


  (check-equal?
    (wrapped "A" (list "body"))
    (list
      "A"
      (list (list "body"))
      "A"))

  (check-equal?
    (wrapped (command "cmd") "body")
    (list
      "\\cmd "
      (list "body")
      "\\cmd ")))
