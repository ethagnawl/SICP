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
