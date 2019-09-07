;; chapter 1

;;; 1.1 the elements of programming

;;;; 1.1.1 expressions

486
(+ 137 349)
(- 1000 334)
(* 5 99)
(/ 10 5)
(+ 2.7 10)

(+ 21 35 12 7)
(* 25 4 12)

(+ (* 3 5) (- 10 6))

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))

(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))

;;;; 1.1.2 naming and the environment

(define size 2)

size
(* 5 size)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))
circumference

;;;; 1.1.4 compound procedures

(define (square x) (* x x))

(square 21)
(square (+ 2 5))
(square (square 3))

(define (sum-of-squares x y)
  (+ (square x) (square y)))
(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(f 5)

;;;; 1.1.6 conditional expressions and predicates

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y)
  (or (> x y) (= x y)))

(define (>= x y)
  (not (< x y)))

;;;; exercises

;;;;; 1.1

10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(+ (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;;;;; 1.2

(/ (+ 5 4 (- 2
             (- 3
                (+ 6
                   (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;;;;; 1.3

(define (g a b c)
  (cond ((and (>= a c) (>= b c)) (sum-of-squares a b))
        ((and (>= a b) (>= c b)) (sum-of-squares a c))
        (else (sum-of-squares b c))))

;;;; 1.1.7 example: square roots by newton's method

;;;; exercises

;;;;; 1.7

(define (sqrt-iter previous-guess new-guess x)
  (if (good-enough? previous-guess new-guess)
      new-guess
      (sqrt-iter new-guess
                 (improve new-guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? previous-guess new-guess)
  (< (abs (- new-guess previous-guess)) 0.001))

(define (sqrt x)
  (sqrt-iter 0 1.0 x))

;;;;; 1.8

(define (cbrt-iter previous-guess new-guess x)
  (if (good-enough? previous-guess new-guess)
      new-guess
      (cbrt-iter new-guess
                 (improve new-guess x)
                 x)))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (cbrt x)
  (cbrt-iter 0.0 1.0 x))

;;;; 1.1.8 procedures as black-box abstractions

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;;; 1.2 procedures and the processes they generate

;;;; 1.2.1 linear recursion and iteration

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;;;; 1.2.2 tree recursion

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;;;; exercises

;;;;; 1.11

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))
(define (f-iter n)
  (if (< n 3)
      n
      (iter 2 1 0 n)))
(define (iter a b c n)
  (if (< n 3)
      a
      (iter (+ a (* 2 b) (* 3 c))
            a
            b
            (- n 1))))

;;;;; 1.12
