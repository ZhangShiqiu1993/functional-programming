#lang planet neil/sicp
(define x (list (list 1 2) (list 3 4)))
x
;(define (reverse items)
;  (if (null? items)
;      nil
;      (cons (reverse (cdr items)) (car items))))
;(reverse x)
(define (deep-reverse items)
  (cond  ((null? items) nil)
         ((not (pair? items)) items)
         (else (cons (deep-reverse (cdr items))
                     (deep-reverse (car items))))))
(deep-reverse x)