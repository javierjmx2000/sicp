;;;;; 1.12

(define (pascal r c)
  (if (or (= c 0)
          (= r c))
      1
      (+ (pascal (- r 1) (- c 1))
         (pascal (- r 1) c))))

;;;; 1.2.3 exponentiation

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

;;;; exercises

;;;;; 1.16
(define (fast-expt b n)
  (fast-expt-iter 1 b n))

(define (fast-expt-iter a b counter)
  (cond ((= counter 0) a)
        ((even? counter) (fast-expt-iter a (square b) (/ counter 2)))
        (else (fast-expt-iter (* a b) b (- counter 1)))))

;;;;; 1.17
(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2))

(define (mul a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mul a (halve b))))
        (else (+ a (mul a (- b 1))))))

;;;;; 1.18

(define (mul a b)
  (mul-iter a b 0))

(define (mul-iter a b sum)
  (cond ((= b 0) sum)
        ((even? b) (mul-iter (double a) (halve b) sum))
        (else (mul-iter a (- b 1) (+ sum a)))))

;;;; 1.2.5 greatest common divisor

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;;; 1.2.6 example: testing for primality

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;;; exercises

;;;;; 1.21

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (search-for-primes a b)
  (timed-prime-test (if (even? a)
                        (+ a 1)
                        a))
  (if (< a b)
      (search-for-primes (+ a 2) b)))

;;;;; 1.23

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

;;; 1.3 formulating abstractions with higher-order procedures

;;;; 1.3.1 procedures as arguments

(define (cube x) (* x x x))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ 1 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;;;; exercises

;;;;; 1.29

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (cond ((= k 0) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))
  (* (sum term 0 inc n)
     (/ h 3)))

;;;;; 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;;;;; 1.31

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-approx n)
  (define (term x)
    (/ (* 4.0 (square x))
       (- (* 4.0 (square x)) 1)))
  (* 2.0 (product term i inc n)))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;;;;; 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner (term a) result))))
  (iter a null-value))

;;;;; 1.33

(define (filtered-accumulate pred combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((pred a) (iter (next a) result))
          (else (iter (next a) (combiner (term a) result)))))
  (iter a null-value))

(define (f a b)
  (filtered-accumulate prime? + 0 square a 1+ b))

(define (g n)
  (define (pred i)
    (= (gcd i n) 1))
  (filtered-accumulate pred * 1 identity 1 1+ n))

;;;; 1.3.2 constructing procedures using lambda


