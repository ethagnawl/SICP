#|
Exercise 1.1
|#

10 ; 10

(+ 5 3 4) ; 12

(- 9 1) ; 8

(/ 6 2) ; 3

(+
  (* 2 4)
  (- 4 6)
) ; 6

(define a 3) ; global variable a
(a) ; 3

(define b (+ a 1)) ; global variable b
(b) ; 4

(+ a b (* a b)) ; 19

(= a b) ; #f

(if (and (> b a) (< b (* a b)))
  b
  a
) ; 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)
) ; 16

(+ 2 ( if (> b a) b a)) ; 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)
) ; 16


#|
Exercise 1.2
|#

(/
 (+ 5 4
    (- 2
       (- 3
	  (+ 6
	     (/ 4 5)))))
 (* 3
    (- 6 2)
    (- 2 7))) ; -37/150


#|
Exercise 1.3
|#

(define (sum-of-squares-of-two-largest-args x y z)
        (let ([two-largest-args (cdr (sort (list x y z) <))]
              [square (lambda (n) (* n n))])
          (foldl + 0 (map square two-largest-args))))

(sum-of-squares-of-two-largest-args 9 1 9) ; 162


#|
Exercise 1.4
|#

(define (a-plus-abs-b a b)
  ; if b is greater than 0, operate normally
  ; if b is less than 0, operate with -, flipping its negation
  ((if (> b 0) + -) a b))

(a-plus-abs-b 10 6) ; 16
(a-plus-abs-b 10 -6) ; 16


#|
Exercise 1.5
|#

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

; because the interpreter uses applicative-order evaluation - evaluate operators
; and then apply operand - test expands into: (if (= 0 0) 0 (p)) ... forever
; because both if conditions are evaluated and p is defined in terms of itself
(test 0 (p))
