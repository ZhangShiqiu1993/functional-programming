#lang planet neil/sicp
(define (cont-frac1 n d k)
  (define (cf-item i)
    (if (= k i)
        (/ (n k) (d k))
        (/ (n k)
           (+ (d k) (cf-item (+ i 1))))))

  (cf-item 1))

(define (cont-frac2 n d k)
  (define (cf-iter i result)
    (if (= i 1)
        (/ (n 1)
           (+ (d 1) result))
        (cf-iter (- i 1)
                 (/ (n i)
                    (+ (d i) result)))))
  (cf-iter k 0))

(define (square x) (* x x))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (square x))))
  (define (d i)
    (- (* i 2.0) 1))
  (cont-frac2 n d k))

(tan 1)
(tan-cf 1 20)