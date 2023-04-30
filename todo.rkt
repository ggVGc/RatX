#lang racket

(define use-inline-todo
  (if with-todo
    (lines
         "\\usepackage[colorinlistoftodos,prependcaption]{todonotes}"
         "\\let\\originaltodo\\todo"
         "\\renewcommand{\\todo}[1]{\\originaltodo[inline]{#1}}")
    ""))
