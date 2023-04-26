#lang racket/base
(require "latex_base.rkt")

(provide 
  documentclass
  article-a4)

(define (documentclass opts type) 
  (command #:opts opts 'documentclass type))

(define (article-a4 [points 11]) 
   (documentclass (list 'a4paper (list points 'pt)) 'article))
