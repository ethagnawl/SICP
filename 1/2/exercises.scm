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


(define (f-iterative n) (A 0 n))

(f-iterative 0) ; 0
(f-iterative 1) ; 2
(f-iterative 2) ; 4
(f-iterative 3) ; 6


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


#|
Exercise 1.11
|#

(define (f-recursive n)
  (cond
    ((< n 3)
     n)
    (else
      (+  (f-recursive (- n 1))
      (* 2 (f-recursive (- n 2)))
      (* 3 (f-recursive (- n 3)))))))

(f-recursive 2) ; 2
(f-recursive 3) ; 4
(f-recursive 10) ; 1892


(define (f-iterative-iterate a b c n)
  ; (write "a:")
  ; (pretty-print a)
  ; (write "b:")
  ; (pretty-print b)
  ; (write "c:")
  ; (pretty-print c)
  ; (write "n:")
  ; (pretty-print n)
  (if (< n 3)
      a
      (f-iterative-iterate
      (+ a (* 2 b) (* 3 c))
      a
      b
      (- n 1))))

(define (f-iterative n) (f-iterative-iterate 2 1 0 n))

(f-iterative 2) ; 2
(f-iterative 3) ; 4
(f-iterative 10) ; 1892


#|
Exercise 1.12
|#

(define (pascal-tri dir row column)
  (write "dir")
  (pretty-print dir)
  (write "row")
  (pretty-print row)
  (write "column")
  (pretty-print column)
  (pretty-print "--------------")

  (cond ((or (< row 1) (< column 1) (> column row)) #f)
        ((or (= column 1) (= column row)) 1)
        (else (+ (pascal-tri "left" (- row 1) (- column 1)) (pascal-tri "right" (- row 1) column)))))

(pascal-tri "" 5 3)
