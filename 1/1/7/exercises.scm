#|
Exercise 1.6
|#

(define (square x) (* x x))

(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

; (define (good-enough? guess x)
;   (< (abs (- (square guess) x)) 0.001))

(define (new-if predicate then-clause else-clause)
  (cond
    (predicate then-clause)
    (else else-clause)))

(new-if (= 2 3) 0 5) ; 5

(define (sqrt-iter guess x)
  (new-if
    (good-enough? guess x) guess
    (sqrt-iter (improve guess x) x)))

; Since new-if is not a special form, the interpreter will attempt to evaluate
; both the (good-enough? 1.0 2.0) operand and the (sqrt-iter (improve 1.0) 2.0)
; operand, before applying the new-if procedure to these arguments.
; - https://github.com/psholtz/MIT-SICP/blob/master/Section-1.1/mit-scheme/exercise1-06.md

(define (sqrt x) (sqrt-iter 1.0 x))

; (sqrt 2.0)


#|
Exercise 1.7
|#

; Modified version of good-enough?, based on fractional
; changes, rather than on an absolute tolerance.
; The fractional tolerance is set at 0.1%.
; - https://github.com/psholtz/MIT-SICP/blob/master/Section-1.1/mit-scheme/exercise1-07.scm
(define (good-enough? guess x)
  (< (abs (- (/ (square guess) x) 1.0)) 0.001))

(sqrt 1) ; 1.0

(sqrt 2) ; 1.4142156862756097

(sqrt 0.01) ; 0.10000052895642693

(sqrt 0.0001) ; 1.0000714038711746e-2


#|
Exercise 1.8
|#

(define (cube x) (* x x x))
;(write (cube 9)) ; 729

(define (good-enough-cube? guess x)
  (define tolerance 0.001)
  (< (abs (- (/ (cube guess) x) 1.0)) tolerance))

(define (improve guess x)
  ; x/y2 + 2y
  ; ---------
  ;   3
  (/
    (+
      (/ x (* guess guess))
      (* 2 guess))
    3))

(define (cube-root-iter guess x)
  (if (good-enough-cube? guess x)
    guess
    (cube-root-iter (improve guess x) x)))

(define (cube-root x) (cube-root-iter 1.0 x))

(cube-root 2) ; 1.259933493449977


#|
Extra Credit
|#

(define (new-sqrt x)

  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))

  (define (improve guess)
      (average guess (/ x guess)))

  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))

  (sqrt-iter 1.0))

(new-sqrt 2) ; 1.4142156862756097
