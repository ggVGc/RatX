#lang racket
(require "latex.rkt")

(provide
  table
  tabular)

(define (table . body)
  (beg-opts "table" "!htb"
    (list
      (command "centering")
      "\n"
      body)))

(define (tabular config headers . entries)
      (beg2 "tabular" config
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
