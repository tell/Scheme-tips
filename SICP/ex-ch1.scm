;;; Implementation for Gauche.

(use slib)
(require 'trace)

(set! debug:max-count 100)
(print debug:max-count)

(print "ex 1.1")

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
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

(print "ex 1.2")

(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

(print "ex 1.3")

(define (ex-1-3 x y z)
  (cond
   ((and (>= x z) (>= y z)) (+ (* x x) (* y y)))
   ((and (>= x y) (>= z y)) (+ (* x x) (* z z)))
   ((and (>= y x) (>= z x)) (+ (* y y) (* z z)))))

(ex-1-3 0 0 0)
(ex-1-3 0 0 1)
(ex-1-3 0 2 1)
(ex-1-3 3 2 1)
(ex-1-3 3 2 3)

(print "ex 1.4")

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 10 11)
(a-plus-abs-b 10 -11)

(print "ex 1.5")

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;;(test 0 (p))

(print "ex 1.6")

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(print "ex 1.7")

(define epsilon 0.0001)
(define initial-value 1.0)

(define (sqrt x)
  (define (improve guess)
    (define (average x y)
      (/ (+ x y) 2))
    (average guess (/ x guess)))
  (define (sqrt-iter last-guess next-guess)
    (define (good-enough?)
      (< (abs (/ (- last-guess next-guess) next-guess))
         epsilon))
    (if (good-enough?)
        next-guess
        (sqrt-iter next-guess (improve next-guess))))
  (trace sqrt-iter)
  (sqrt-iter initial-value (improve initial-value)))

(define (sq x)
  (* x x))

(print "sqrt(2) = " (sqrt 2))
(print "sq(sqrt(2)) = " (sq (sqrt 2)))
(print "sqrt(3) = " (sqrt 3))
(print "sq(sqrt(3)) = " (sq (sqrt 3)))

(print "ex 1.8")

(define (cube-root x)
  (define (improve guess)
    (/ (+ (/ x guess guess) guess guess) 3))
  (define (cube-root-iter guess)
    (define (good-enough? guess)
      (< (abs (- (* guess guess guess) x))
         epsilon))
    (if (good-enough? guess)
        guess
        (cube-root-iter (improve guess))))
  (cube-root-iter initial-value))

(define (cube x)
  (* x x x))

(print "cube-root(2) = " (cube-root 2))
(print "cube(cube-root(2)) = " (cube (cube-root 2)))
(print "cube-root(3) = " (cube-root 3))
(print "cube(cube-root(3)) = " (cube (cube-root 3)))

(define (cube-root-1 x)
  (define (improve guess)
    (/ (+ (/ x guess guess) guess guess) 3))
  (define (cube-root-iter last-guess next-guess)
    (define (good-enough?)
      (< (abs (/ (- last-guess next-guess) last-guess))
         epsilon))
    (if (good-enough?)
        next-guess
        (cube-root-iter next-guess (improve next-guess))))
  (cube-root-iter initial-value (improve initial-value)))

(print "cube-root-1(2) = " (cube-root-1 2))
(print "cube(cube-root-1(2)) = " (cube (cube-root-1 2)))
(print "cube-root-1(3) = " (cube-root-1 3))
(print "cube(cube-root-1(3)) = " (cube (cube-root-1 3)))

(print "ex 1.9")

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (plus-1 a b)
  (if (= a 0)
      b
      (inc (plus-1 (dec a) b))))
(trace plus-1)

(define (plus-2 a b)
  (if (= a 0)
      b
      (plus-2 (dec a) (inc b))))
(trace plus-2)

(plus-1 5 6)
(plus-2 5 6)

(print "ex 1.10")

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
;;(trace A)

(print "A(1, 10) = " (A 1 10))
(print "A(2, 4) = " (A 2 4))
(print "A(3, 3) = " (A 3 3))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* k n n))

(print "ex 1.11")

(define (trib-r x)
  (if (< x 3)
      x
      (+ (trib-r (- x 1))
         (* 2 (trib-r (- x 2)))
         (* 3 (trib-r (- x 3))))))
(trace trib-r)

(define (trib-i x)
  (define (trib-iter n a b c)
    (if (= n 0)
        c
        (trib-iter (- n 1)
                   (+ a (* 2 b) (* 3 c))
                   a
                   b)))
  (trace trib-iter)
  (trib-iter x 2 1 0))

(print "trib-r(0) = " (trib-r 0))
(print "trib-i(0) = " (trib-i 0))
(print "trib-r(1) = " (trib-r 1))
(print "trib-i(1) = " (trib-i 1))
(print "trib-r(2) = " (trib-r 2))
(print "trib-i(2) = " (trib-i 2))
(print "trib-r(3) = " (trib-r 3))
(print "trib-i(3) = " (trib-i 3))
(print "trib-r(10) = " (trib-r 10))
(print "trib-i(10) = " (trib-i 10))
(print "trib-i(10) = " (trib-i 30))

(print "ex 1.12")

(define (comb-r n k)
  (cond ((= k 0) 1)
        ((= n k) 1)
        (else (+ (comb-r (- n 1) k)
                 (comb-r (- n 1) (- k 1))))))
(trace comb-r)

(define (comb-i n k)
  (define (comb-iter x n k)
    (if (= k 0)
        x
        (comb-iter (* x
                      (/ n k))
                   (- n 1)
                   (- k 1))))
  (trace comb-iter)
  (comb-iter 1 n k))

(print (comb-r 0 0))

(print (comb-i 0 0))

(print (comb-r 1 0))
(print (comb-r 1 1))

(print (comb-i 1 0))
(print (comb-i 1 1))

(print (comb-r 2 0))
(print (comb-r 2 1))
(print (comb-r 2 2))

(print (comb-i 2 0))
(print (comb-i 2 1))
(print (comb-i 2 2))

(print (comb-r 3 0))
(print (comb-r 3 1))
(print (comb-r 3 2))
(print (comb-r 3 3))

(print (comb-i 3 0))
(print (comb-i 3 1))
(print (comb-i 3 2))
(print (comb-i 3 3))
