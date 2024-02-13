#lang racket
(require "latex.rkt")

(provide
  table
  table-env
  tabular)

(define (add-label content)
  (if (null? content)
    ""
    (label content)))

(define (add-caption content)
  (if (null? content)
    ""
    (list
     (caption content)
     (command 'vspace "3mm"))))
 

(define (table headers #:config [config null] #:caption [cap null] #:label [lab null] . body)
  (table-env
    (add-caption cap) 
    (if (null? config)
      (apply tabular headers body)
      (apply tabular headers #:config config body))
    (add-label lab))) 


(define (table-env #:opt [opt "!htb"] . body)
  (beg #:opt opt "table"
    (list
      (command "centering")
      "\n"
      body)))

(define (tabular headers 
          #:config [config (make-list (length headers) "l")] 
          . entries)

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

