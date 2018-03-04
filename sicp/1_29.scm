#lang planet neil/sicp
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (cube x) (* x x x))
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))



(define (sheepson f a b n)
  
  (define (sum-y a b gap)
    (sum y a (lambda (x)(+ x gap)) b))

  (define h (/ (- b a) n))

  (define (y k)
    (f (+ a
          (* k h))))
  
  (* (/ h 3)
     (+ (y 0)
        (y n)
        (* 4
           (sum-y 1 (- n 1) 2))
        (* 2
           (sum-y 2 (- n 2) 2))
        )))