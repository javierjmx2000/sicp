;; 2.5

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))
(define (car x) (iter 2 0 x))
(define (cdr x) (iter 3 0 x))
(define (iter n count x)
  (if (= 0 (remainder x n))
      (iter n
            (1+ count)
            (/ x n))
      count))
