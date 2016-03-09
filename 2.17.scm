#lang planet neil/sicp
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
;(list-ref squares 3)

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ count 1))))
  (length-iter items 0))

;(define (length items)
;  (if (null? items)
;      0
;      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))
;(length odds)

;(define (append list1 list2)
;  (if (null? list2)
;      list1
;      (append (cons list1 (car list2)) (cdr list2))))
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
;(append squares odds)
;(append odds squares)

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))
(last-pair (list 23 72 149 34))