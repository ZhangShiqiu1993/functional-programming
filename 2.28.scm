#lang planet neil/sicp
(define x (list (list 1 2) (list 3 4)))

(define (leave? item) (not (pair? item)))

(define (fringe tree)
    (cond 
        ((null? tree) nil)
        ((leave? tree) (display tree))
        (else (fringe (car tree))
              (fringe (cdr tree))
        )
    )
)
(fringe x)
(fringe (list x x))