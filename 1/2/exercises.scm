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
