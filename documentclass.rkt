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
  (list 
      'float
      'amsmath
      'amssymb))

(define (article #:points [points 11] #:packages [packages default-packages])
  (list
   (documentclass (list 'a4paper (list points 'pt)) 'article)
   (map (curry command 'usepackage) packages )))

; TODO: Deprecate
(define article-a4 article)
