#|
Exercise 1.9
|#

(define (dec x) (- x 1))
; (dec 2) ; 1

(define (inc x) (+ x 1))
; (inc 1) ; 2

(define (my-plus a b)
  ; linear recursive
  (if (= a 0)
    b
    (inc (my-plus (dec a) b))))

; (my-plus 4 5)
; (my-plus 4 5)
; (inc (my-plus (dec 4) 5))
; (inc (my-plus 3 5))
; (inc (inc (my-plus (dec 3) 5)))
; (inc (inc (my-plus 2 5)))
; (inc (inc (inc (my-plus (dec 2) 5))))
; (inc (inc (inc (my-plus 1 5))))
; (inc (inc (inc (inc (my-plus (dec 1) 5)))))
; (inc (inc (inc (inc (my-plus 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

(define (my-other-plus a b)
  ; linear iterative
  (if (= a 0)
    b
    (my-other-plus (dec a) (inc b))))

; (my-other-plus 4 5)
; (my-other-plus (dec 4) (inc 5))
; (my-other-plus 3 6)
; (my-other-plus (dec 3) (inc 6))
; (my-other-plus 2 7)
; (my-other-plus (dec 2) (inc 7))
; (my-other-plus 1 8)
; (my-other-plus (dec 1) (inc 8))
; (my-other-plus 0 9)
; 9


#|
Exercise 1.10
|#
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))))


(A 1 10) ; 1024
(A 2 4) ; 65536
(A 3 3) ; 65536


(define (f n) (A 0 n))

(f 0) ; 0
(f 1) ; 2
(f 2) ; 4
(f 3) ; 6


(define (g n) (A 1 n))

(g 0) ; 0
(g 1) ; 2
(g 2) ; 4
(g 3) ; 8


(define (h n) (A 2 n))

(h 0) ; 0
(h 1) ; 2
(h 2) ; 4
(h 3) ; 16
(h 4) ; 65536


(define (k n) (* 5 n n))

(k 0) ; 0
(k 1) ; 5
(k 2) ; 20
(k 3) ; 45
(k 4) ; 80
