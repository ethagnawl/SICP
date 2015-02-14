#|
Exercise 1.29
|#

; http://www.billthelizard.com/2010/04/sicp-exercise-129-integration-using.html

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((odd? k) 4)
             ((or (= k 0) (= k n)) 1)
             ((even? k) 2))
       (y k)))
  (/ (* h (sum term 0 inc n)) 3))

(define (cube x) (* x x x))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)


#|
Exercise 1.30
|#

; http://www.billthelizard.com/2010/04/sicp-exercise-130-iterative-sums.html

(define (iterative-sum func a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (func a) result))))
  (iter a 0))

; (define (inc x) (+ 1 x))
; (iterative-sum identity 0 inc 10)

(define (new-integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (iterative-sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (new-simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((odd? k) 4)
             ((or (= k 0) (= k n)) 1)
             ((even? k) 2))
       (y k)))
  (/ (* h (iterative-sum term 0 inc n)) 3))

(define (cube x) (* x x x))

(new-integral cube 0 1 0.01)
(new-integral cube 0 1 0.001)


#|
Exercise 1.31
|#

; http://www.billthelizard.com/2010/05/sicp-exercise-131-product-of-series.html

(define (square x) (* x x))

(define (wallis-pi func n)
  (define (term x)
    (/ (* 4.0 (square x))
       (- (* 4.0 (square x)) 1)))
  (* 2.0 (func term 1 inc n)))

(define (linear-recursive-product term a next b)
  (if (> a b)
    1
    (*
      (term a)
      (linear-recursive-product term (next a) next b))))

; (* 1 (* 2 (* 3 (* 4 5))))

(define (inc x) (+ 1 x))

(define (linear-recursive-factorial x) (linear-recursive-product identity 1 inc x))

(linear-recursive-factorial 5) ; 120

(wallis-pi linear-recursive-product 10) ; 3.0677038066434994

(define (linear-iterative-product term a next b)
  (letrec ([iter (lambda (a result)
                   (if (> a b)
                     result
                     (iter (next a) (* (term a) result))))])
    (iter a 1)))

(define (linear-iterative-factorial x)
  (linear-iterative-product identity 1 inc x))

(linear-iterative-factorial 5) ; 120

(wallis-pi linear-iterative-product 10) ; 3.0677038066434985
                                        ; which differs sligtly from the
                                        ; linear-recursive-product version


#|
Exercise 1.32
|#

(define (inc x) (+ 1 x))

(define (linear-recursive-accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner
      (term a)
      (linear-recursive-accumulate combiner null-value term (next a) next b) )))

; sum
((lambda (a b) (linear-recursive-accumulate + 0 identity a inc b)) 0 10) ; 55

; product
((lambda (a b) (linear-recursive-accumulate * 1 identity a inc b)) 1 5) ; 120

(define (linear-iterative-accumulate combiner null-value term a next b)
  (letrec ([iter (lambda (a result)
                   (if (> a b)
                     result
                     (iter (next a) (combiner (term a) result))))])
    (iter a null-value)))

; sum
((lambda (a b) (linear-iterative-accumulate + 0 identity a inc b)) 0 10) ; 55

; product
((lambda (a b) (linear-iterative-accumulate * 1 identity a inc b)) 1 5) ; 120
