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

(define (n i) 1.0)
(define (d i)
  (if (= (remainder i 3) 2)
      (/ (+ i 1) 3)
      1))
(define e (+ (cont-frac1 n d 10) 2.0))