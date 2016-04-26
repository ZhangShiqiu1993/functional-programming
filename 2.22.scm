#lang planet neil/sicp
(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items nil))

;(define (square-list items) (map square items))

(square-list (list 1 2 3 4))