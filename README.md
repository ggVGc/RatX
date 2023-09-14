# RatX
Write LaTeX using Racket...

Currently a collection of wild experiments for generating latex from Racket, actively used by me to reduce the tedium of writing physics reports,
while also learning Racket.

Example:
```racket
; vim: ft=racket.ratx

#lang at-exp s-exp (file "~/stuff/RatX/display-all.rkt")

(require (file "~/stuff/RatX/RatX.rkt"))
(require (file "~/stuff/RatX/convenience.rkt"))

(article-a4)
(usepackage 'tabularx)
(usepackage 'booktabs)
(usepackage 'siunitx)

(doc-begin
  "A very interesting topic - Science from the soul"
  "Lord Hypnos")
make-title

@section{A very catchy introduction}
@~{Fluffy content making you regret downloading and opening this document,
   but you might as well finish the introduction and skim the sections at least.}

@section{The first thing we want to present to you}
@~{Here we will discuss some things you probably don't actually care about.}

@subsection{Elaboration on a topic you'd rather skip}
@~{Information that might show up on the final exam, but doesn't actually interest you particularily at this point in time.}

@section{Results that actually might interest you}
@~{Here we will simply give you an equation and let you figure it out.}
(equation
  (abs a + b) = (frac
                  (e ^ pi)
                  (lam (_ hbar))))

@~{and a table:}
(table
  #:caption "Extremely unique and useful data"
  #:label "table:something-to-reference"
  (list "A thing" "And a value")
  (list "Measurement with error" (SI-err 42 13 'micro 'kelvin))
  (list 'everything 'futile))

doc-end

```
Output:
```latex
\documentclass[a4paper,11pt]{article}
\usepackage{tabularx}
\usepackage{booktabs}
\usepackage{siunitx}
\title{A very interesting topic - Science from the soul}
\author{Lord Hypnos}
\date{}
\begin{document}
\maketitle 
\section{A very catchy introduction}
Fluffy content making you regret downloading and opening this document,
but you might as well finish the introduction and skim the sections at least.
\section{The first thing we want to present to you}
Here we will discuss some things you probably don't actually care about.
\subsection{Elaboration on a topic you'd rather skip}
Information that might show up on the final exam, but doesn't actually interest you particularily at this point in time.
\section{Results that actually might interest you}
Here we will simply give you an equation and let you figure it out.
\begin{equation}
\left|a+b\right|=\frac{e^\pi }{\lambda _{\hbar }}
\end{equation}
and a table:
\begin{table}[!htb]
\centering 
\begin{tabular}{ll}
\toprule A thing&And a value\\ \midrule 
Measurement with error & \SI{42\pm 13}{\micro\kelvin}\\
 everything & futile\\
 \bottomrule 
\end{tabular}\caption{Extremely unique and useful data}\label{table:something-to-reference}
\end{table}
\end{document}
```
