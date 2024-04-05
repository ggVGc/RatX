#lang racket

(provide
 item-to-string
 expand-body
 intersperse
 wrapped
 command
 lines
 usepackage
 usepackages)

(define (debug msg x)
  (printf "~a: ~a~n" msg x)
  x)

(define (intersperse separator lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (cons (car lst) (cons separator (intersperse separator (cdr lst))))))

(define (item-to-string val)
  (cond
    [(string? val) val]
    [(number? val) (number->string val)]
    [(symbol? val) (symbol->string val)]
    [(procedure? val) (item-to-string (val))]
    ; [(procedure? val) (raise-argument-error 'item-to-string "non-procedure" val)]
    [else '()]))


(define (expand-body . arg)
  (match (item-to-string arg)
    ['()
     (string-join (map item-to-string (flatten arg)) "")]
    [x x]))



(define (command name #:opts [opts null] . args)
  (define wrapped-opts
    (if (null? opts)
        null
        (list "[" (intersperse "," opts) "]")))

  (define wrapped-args
    (if (null? args)
        (list " ")
        (for/list ([a args])
          (list "{" a "}"))))

  (append
   (list "\\" name) 
   wrapped-opts
   wrapped-args))


(define (wrapped a body)
  (list a body a))

(define (lines . entries)
  (intersperse "\n" entries))

(define (usepackage name . opts)
  (command #:opts opts 'usepackage name))

(define (usepackages . packages)
  (define (gen-package-uses package-list)
    (lines
     (intersperse "\n"
                  (map
                   (λ (entry)
                     (match entry
                       [(list name opts)
                        (apply usepackage name opts)]
                       [name
                        #:when (not (list? name))
                        (usepackage name)]))
                   package-list))))
  (match packages
    [(list pkgs)
     (gen-package-uses pkgs)]
    [pkgs (gen-package-uses pkgs)])
  )

(module+ test
  (require rackunit)
  ; (define (show x) 
  ;   (string-join (map item-to-string x) ""))

  (test-case "expand-body"
             (check-equal?
              (expand-body (list 1 2))
              "12") 

             (check-equal?
              (expand-body (list 1 2))
              "12"))


  (test-case "command"
             (check-equal?
              (command "cmd")
              (list "\\" "cmd" " "))

             (check-equal?
              (command "cmd" "arg")
              (list  "\\" "cmd" 
                     (list "{" "arg" "}")))

             (check-equal?
              (command "cmd" 12 'b)

              (list  "\\" "cmd"
                     (list "{" 12 "}")
                     (list "{" 'b "}"))))

  (test-case "wrapped"
             (let*
                 ([thing (list "body")])
               (check-equal?
                (wrapped "A" thing)
                (list "A" thing "A")))

             (let*
                 ([wrapper (command "cmd")])
               (check-equal?
                (wrapped wrapper "body")
                (list wrapper "body" wrapper)))))




