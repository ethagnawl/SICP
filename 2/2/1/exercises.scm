#|
Exercise 2.17
|#

(define (last-pair l)
  (letrec ([last-pair-iter (lambda (head tail)
                            (if (null? tail)
                              head
                              (last-pair-iter (car tail) (cdr tail))))])
    (last-pair-iter (car l) (cdr l))))

(last-pair (list 23 72 149 32))


