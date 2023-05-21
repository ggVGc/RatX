#lang racket
(require "latex_base.rkt")

(define use-inline-todo
  (lines
    "\\usepackage[colorinlistoftodos,prependcaption]{todonotes}"
    "\\let\\originaltodo\\todo"
    "\\renewcommand{\\todo}[1]{\\originaltodo[inline]{#1}}"))
