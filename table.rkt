#lang racket
(require "latex.rkt")

(provide
  table
  tabular)

(define (table . body)
  (beg #:opt "!htb" "table"
    (list
      (command "centering")
      "\n"
      body)))

(define (tabular config headers . entries)
      (beg #:arg config "tabular"
        (list
          (command "toprule")
          (intersperse "&" headers)
          (command "\\")
          (command "midrule")
          "\n"
          (map
            (lambda (x) 
              (list 
                (intersperse " & " x)
                (command "\\\n")))
            entries)
          (command "bottomrule"))))
