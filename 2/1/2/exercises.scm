#|
Exercise 2.1
|#

; http://www.billthelizard.com/2010/09/sicp-21-rational-numbers.html

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
      (cons (/ (* n -1) g) (/ (* d -1) g))
      (cons (/ n g) (/ d g)))))

(print-rat (make-rat 1 2))


#|
Exercise 2.2
|#

; http://www.billthelizard.com/2010/09/sicp-22-line-segments-in-plane.html

(define (make-segment a b) (cons a b))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (make-point
              (/
                (+
                  (x-point (start-segment s))
                  (x-point (end-segment s)))
              2)
              (/
                (+
                  (y-point (start-segment s))
                  (y-point (end-segment s)))
              2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

; (–1, 2) and (3, –6)
(define start (make-point -1 2))
(define end (make-point 3 -6))
(define segment (make-segment start end))
(define midpoint (midpoint-segment segment))
(print-point midpoint) ; (1, -2)

; (6.4, 3) and (–10.7, 4)
(define start (make-point 6.4 3))
(define end (make-point -10.7 4))
(define segment (make-segment start end))
(define midpoint (midpoint-segment segment))
(print-point midpoint) ; (-2.1499999999999995,7/2)


#|
Exercise 2.3
|#

; http://www.billthelizard.com/2010/10/sicp-23-rectangles-in-plane.html

(define (make-rect a b) (cons a b))

(define (rect-width r)
  (abs (- (x-point (car r)) (x-point (cdr r)))))

(define (rect-height r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))

(define (rect-perimeter r)
  (* 2 (+ (rect-width r) (rect-height r))))

(define (rect-area r)
  (* (rect-width r) (rect-height r)))

(define a (make-point 0 0))
(define b (make-point 2 10))
(define r (make-rect a b))
(display (rect-perimeter r))
(display (rect-area r))

(define (make-alt-rect a w h) (cons a (cons w h)))

(define (alt-rect-width r) (car (cdr r)))

(define (alt-rect-height r) (cdr (cdr r)))

(define alt-a (make-point 0 0))
(define alt-rec (make-alt-rect alt-a 2 10))
(display (rect-perimeter alt-rec))
(display (rect-area alt-rec))


#|
Exercise 2.4
|#

(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-car z)
  (z (lambda (p q) p)))

(display (my-car (my-cons 1 2))) ; 1

(define (my-cdr z)
  (z (lambda (p q) q)))

(display (my-cdr (my-cons 1 2))) ; 2


#|
Exercise 2.5
|#

; http://www.billthelizard.com/2010/10/sicp-25-representing-pairs-as-product.html

(define (num-divs n d)
  (define (iter x result)
    (if (= 0 (remainder x d))
      (iter (/ x d) (+ 1 result))
      result))
  (iter n 0))

; (num-divs 23328 2) ; 5

; (num-divs 23328 3) ; 6

(define (my-new-cons a b)
  (* (expt 2 a)
     (expt 3 b)))

; (my-new-cons 1 2) ; 18

(define (my-new-car x)
  (num-divs x 2))

(my-new-car (my-new-cons 1 2)) ; 1

(define (my-new-cdr x)
  (num-divs x 3))

(my-new-cdr (my-new-cons 1 2)) ; 2


#|
Exercise 2.6
|#

; http://www.billthelizard.com/2010/10/sicp-26-church-numerals.html

(define (inc n) (+ n 1))

(define (zero f) (lambda (x) x))

(define (add-1 n)
  (lambda (f) (
    lambda (x)
      (f ((n f) x)))))

((zero inc) 0) ; 0

((zero inc) 1) ; 1

((zero inc) 2) ; 2

; (define one (add-1 zero))

; ((one inc) 0) ; 1

; ; same as (((add-1 zero) inc) 1) ; 2
; ((one inc) 1) ; 2

; (define two (add-1 one))

; ((two inc) 0) ; 2
; ((two inc) 1) ; 3

(define (one f) (lambda (x) (f x)))

(define (two f) (lambda (x) (f (f x))))

((one inc) 0) ; 1
((one inc) 1) ; 2

((two inc) 3) ; 5
((two inc) 5) ; 7

(define (add-church m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

(define three (add-church one two))

((three inc) 0) ; 3

(define four (add-church two two))

((four inc) 0)

(define seven (add-church three four))

((seven inc) 0)


#|
Exercise 2.7
|#

; http://www.billthelizard.com/2010/12/sicp-27-211-extended-exercise-interval.html

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (lower-bound p) (car p))

(define (upper-bound p) (cdr p))

(define a (make-interval 5 10))

(define b (make-interval 10 20))

(define c (make-interval 5 20))

(add-interval a b) ; (15 . 30)


#|
Exercise 2.8
|#

; http://www.billthelizard.com/2010/12/sicp-27-211-extended-exercise-interval.html

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define d (make-interval 1 10))

(define e (make-interval 50 100))

(define f (make-interval 5 20))

(sub-interval e d) ; (40 . 99)

(sub-interval d e) ; (-99 . -40)

(sub-interval d f) ; (-19 . 5)

(sub-interval f d) ; (-5 . 19)


#|
Exercise 2.9
|#

; http://www.billthelizard.com/2010/12/sicp-27-211-extended-exercise-interval.html

(define aa (make-interval 2 4))

(define bb (make-interval 5 10))

(define cc (make-interval 10 15))

(mul-interval aa bb) ; (10 . 40)

(mul-interval aa cc) ; (20 . 60)

; The intervals b and c have the same width, but when we multiply each of them
; by interval a, the resulting intervals have different widths. This means that
; the width of the product of two intervals cannot be a function of only the
; widths of the operands.


#|
Exercise 2.10
|#

; http://www.billthelizard.com/2010/12/sicp-27-211-extended-exercise-interval.html

(define (spans-zero? y)
  (and (<= (lower-bound y) 0)
       (>= (upper-bound y) 0)))

(define (div-interval x y)
  (if (spans-zero? y)
    (error "Error: The denominator should not span 0.")
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

(define aaa (make-interval 2 5))

(define bbb (make-interval -2 2))

(div-interval aaa bbb) ; Error: The denominator should not span 0.


#|
Exercise 2.11
|#

; http://www.billthelizard.com/2010/12/sicp-27-211-extended-exercise-interval.html

(define (mul-interval x y)
  (let ((xlo (lower-bound x))
        (xhi (upper-bound x))
        (ylo (lower-bound y))
        (yhi (upper-bound y)))
    (cond ((and (>= xlo 0)
                (>= xhi 0)
                (>= ylo 0)
                (>= yhi 0))
           ; [+, +] * [+, +]
           (make-interval (* xlo ylo) (* xhi yhi)))
          ((and (>= xlo 0)
                (>= xhi 0)
                (<= ylo 0)
                (>= yhi 0))
           ; [+, +] * [-, +]
           (make-interval (* xhi ylo) (* xhi yhi)))
          ((and (>= xlo 0)
                (>= xhi 0)
                (<= ylo 0)
                (<= yhi 0))
           ; [+, +] * [-, -]
           (make-interval (* xhi ylo) (* xlo yhi)))
          ((and (<= xlo 0)
                (>= xhi 0)
                (>= ylo 0)
                (>= yhi 0))
           ; [-, +] * [+, +]
           (make-interval (* xlo yhi) (* xhi yhi)))
          ((and (<= xlo 0)
                (>= xhi 0)
                (<= ylo 0)
                (>= yhi 0))
           ; [-, +] * [-, +]
           (make-interval (min (* xhi ylo) (* xlo yhi))
                          (max (* xlo ylo) (* xhi yhi))))
          ((and (<= xlo 0)
                (>= xhi 0)
                (<= ylo 0)
                (<= yhi 0))
           ; [-, +] * [-, -]
           (make-interval (* xhi ylo) (* xlo ylo)))
          ((and (<= xlo 0)
                (<= xhi 0)
                (>= ylo 0)
                (>= yhi 0))
           ; [-, -] * [+, +]
           (make-interval (* xlo yhi) (* xhi ylo)))
          ((and (<= xlo 0)
                (<= xhi 0)
                (<= ylo 0)
                (>= yhi 0))
           ; [-, -] * [-, +]
           (make-interval (* xlo yhi) (* xlo ylo)))
          ((and (<= xlo 0)
                (<= xhi 0)
                (<= ylo 0)
                (<= yhi 0))
           ; [-, -] * [-, -]
           (make-interval (* xhi yhi) (* xlo ylo))))))

(define aaaa (make-interval 2 4))

(define bbbb (make-interval -2 4))

(define cccc (make-interval -4 -2))

(mul-interval aaaa aaaa) ; (4 . 16)
(mul-interval aaaa bbbb) ; (-8 . 16)
(mul-interval aaaa cccc) ; (-16 . -4)
(mul-interval bbbb aaaa) ; (-8 . 16)
(mul-interval bbbb bbbb) ; (-8 . 16)
(mul-interval bbbb cccc) ; (-16 . 8)
(mul-interval cccc aaaa) ; (-16 . -4)
(mul-interval cccc bbbb) ; (-16 . 8)
(mul-interval cccc cccc) ; (4 . 16)


#|
Exercise 2.12
|#

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))

(define (percent i)
  (* 100.0 (/ (width i) (center i))))

(define a (make-center-percent 5 20))

a ; (4.0 . 6.0)
(center a) ; 5.0
(width a) ; 1.0
(percent a) ; 20.0

