#lang racket

(require (file "./latex_base.rkt"))
(require (file "./simple_commands.rkt"))

(provide
 use-inline-todo
 todo)

(define (todo . content)
  (command 'todo content))

(define (use-inline-todo)
  (lines
   (usepackage 'todonotes
               'colorinlistoftodos
               'prependcaption)

   (list (command 'let) (command 'originaltodo) (command 'todo))
   (list
    (command 'renewcommand (command 'todo))
    "[1]{"
    (command 'originaltodo
             #:opts (list
                     'inline
                     "backgroundcolor = pink")
             "#1")
    "}")))
