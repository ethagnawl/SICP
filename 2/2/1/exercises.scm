#|
Exercise 2.17
|#

(define (last-pair l)
  (letrec ([last-pair-iter (lambda (head tail)
                            (if (null? tail)
                              head
                              (last-pair-iter (car tail) (cdr tail))))])
    (last-pair-iter (car l) (cdr l))))

(last-pair (list 23 72 149 32)) ; 32


#|
Exercise 2.18
|#

(define (my-reverse l)
  (letrec ([iter (lambda (inner-l memo)
                   (if (null? inner-l)
                     memo
                     (iter (cdr inner-l) (cons (car inner-l) memo))))])
  (iter l (list))))

(my-reverse (list 23 72 149 32)) ; (32 149 72 23)

