#lang racket

(require (file "./latex_base.rkt"))

(provide
  use-inline-todo
  todo)

(define (todo . content)
  (command 'todo content))

(define (use-inline-todo)
  (lines
       "\\usepackage[colorinlistoftodos,prependcaption]{todonotes}"
       "\\let\\originaltodo\\todo"
       "\\renewcommand{\\todo}[1]{\\originaltodo[inline]{#1}}"))
