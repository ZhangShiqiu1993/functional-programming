#lang planet neil/sicp
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define x (cons (list 1 2) (list 3 4)))
;(length x)

(define (count-leaves items)
  (cond ((null? items) 0)
        ((not (pair? items)) 1)
        (else (+ (count-leaves (car items))
                 (count-leaves (cdr items))))))

(count-leaves x)

(list x x)