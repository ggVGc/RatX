#lang racket/base
(require "latex_base.rkt")
(require (only-in racket
                  curry))

(provide
 documentclass
 article
 article-a4)

(define (documentclass opts type)
  (command #:opts opts 'documentclass type))

(define default-packages
  '((inputenc (utf8))
    #| Prevent figures and tables from ending up outside their sections. |#
    (placeins (section))
    #| Begin paragraphs with an empty line rather than an indent. |#
    (parskip (parfill))
    graphicx
    float
    mathtools
    amsmath
    amsthm
    amsfonts
    amssymb
    cancel
    tabularx
    booktabs
    siunitx
    bm))

(define (article #:points [points 11] #:packages [packages default-packages])
  (list
   (documentclass (list 'a4paper (list points 'pt)) 'article)
   (apply usepackages packages)))

; TODO: Deprecate
(define article-a4 article)
