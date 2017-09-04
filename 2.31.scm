#lang planet neil/sicp
(define (leave? tree) (not (pair? tree)))
(define (tree-map proc tree)
    (cond ((null? tree) nil)
          ((leave? tree) (proc tree))
          (else (cons (tree-map proc (car tree))
                      (tree-map proc (cdr tree))))))
(define (square item) (* item item))
(define (square-tree tree)(tree-map square tree))
(define ans (square-tree
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7))))
(display ans)