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


#|
Exercise 1.33
|#

; http://www.billthelizard.com/2010/05/sicp-exercise-133-filtered-accumulator.html

(define (square x) (* x x))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (fast-prime? n)
  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (define (next x)
        (if (= x 2) 3 (+ x 2)))
      (define (divides? a b)
        (= (remainder b a) 0))
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (next test-divisor)))))
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (filtered-accum filter combiner null-value term a next b)
  (if (> a b)
    null-value
    (if (filter a)
      (combiner (term a)
                (filtered-accum filter combiner null-value term (next a) next b))
      (filtered-accum filter combiner null-value term (next a) next b))))

; sum even numbers
((lambda (a b) (filtered-accum even? + 0 identity a inc b)) 0 10) ; 30

; find the sum of the squares of the prime numbers in a given range
((lambda (a b) (filtered-accum fast-prime? + 0 square a inc b)) 2 3) ; 13

; find the product of all the positive integers less than n that are relatively prime to n
(define (product-of-coprimes n)
  (define (coprime? i) (= 1 (gcd i n)))
  (filtered-accum coprime? * 1 identity 1 inc (- n 1)))

(product-of-coprimes 10) ; 189
(product-of-coprimes 11) ; 362880


#|
Exercise 1.34
|#

(define (f g) (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))

(f f)
; evaluation:
; (f f)
; (f 2)
; (2 2)


#|
Exercise 1.35
|#

; http://www.billthelizard.com/2010/07/sicp-exercise-135-fixed-points-and.html

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 2.0)


#|
Exercise 1.36
|#

; http://www.billthelizard.com/2010/07/sicp-exercise-136-fixed-points-and.html

(define (average x y) (/ (+ x y) 2))

(define (fixed-point-with-logging f first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point-with-logging (lambda (x) (/ (log 1000) (log x))) 2.0)
(fixed-point-with-logging (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)


#|
Exercise 1.37
|#

; http://www.billthelizard.com/2010/07/sicp-137-138-and-139-continued.html

(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
      (/ (n i) (+ (d i) (frac (+ i 1))))
      (/ (n i) (d i))))
  (frac 1))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 5) ; 0.625
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 8) ; 0.6176470588235294
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 9) ; 0.6181818181818182
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10) ; 0.6179775280898876

(define (cont-frac-iter n d k)
  (define (frac-iter i result)
    (if (= i 0)
      result
      (frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
  (frac-iter (- k 1) (/ (n k) (d k))))

(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 5) ; 0.625
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 8) ; 0.6176470588235294
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 9) ; 0.6181818181818182
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10) ; 0.6179775280898876


#|
Exercise 1.38
|#

; http://www.billthelizard.com/2010/07/sicp-137-138-and-139-continued.html

(define (d i)
  (if (not (= 0 (remainder (+ i 1) 3)))
    1
    (* 2 (/ (+ i 1) 3))))

(d 8) ; 6

(define e (+ 2 (cont-frac (lambda (i) 1.0) d 10)))

(display e) ; 2.7182817182817183


#|
Exercise 1.39
|#

; http://www.billthelizard.com/2010/07/sicp-137-138-and-139-continued.html

(define (square x) (* x x))

(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
      x
      (- (square x))))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac n d k))

(tan (/ pi 6))        ; 0.5773502691896257

(tan-cf (/ pi 6) 10)  ; 0.5773502691896257


#|
Exercise 1.40
|#

; http://www.billthelizard.com/2010/08/sicp-140-zeros-of-cubic-function.html

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

(newtons-method (cubic 3 -2.4 6) 1) ; -3.9813366488305104


#|
Exercise 1.41
|#

(define (double f) (lambda (x) (f (f x))))
(define (adder x) (lambda (xx) (+ x xx)))
(define (add1 x) ((adder 1) x))
(add1 1) ; 2
((double add1) 2) ; 4
(((double (double double)) (lambda (x) (+ 1 x))) 5) ; 21


#|
Exercise 1.42
|#

(define (compose f g) (lambda (x) (f (g x))))

((compose (lambda (x) (* x x)) (lambda (x) (+ 1 x))) 6) ; 49


#|
Exercise 1.43
|#

(define (repeated f n)
  (if (= n 1)
    f
    ; why does this variant cause an out of memory error?!
    ; (compose f (repeated f (- 1 n)))
    (compose f (repeated f (- n 1)))))

(define (square n) (* n n))
((repeated square 2) 6) ; 1296


#|
Exercise 1.44
|#

; http://www.billthelizard.com/2010/08/sicp-144-smoothing-function.html

(define (smooth f dx)
  (lambda (x) (/
               (+
                 (f x)
                 (f (+ x dx))
                 (f (- x dx)))
               3)))

((smooth sin 0.7) (/ pi 2)) ; 0.8432281248563256

(define (n-fold-smooth f dx n) (repeated (smooth f dx) n))

((n-fold-smooth sin 0.7 2) (/ pi 2)) ; 0.6297176112540723
