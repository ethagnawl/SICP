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


