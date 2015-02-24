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


#|
Exercise 2.19
|#

(define (first-denomination lst) (car lst))

(define (except-first-denomination lst) (cdr lst))

(define (no-more? lst) (null? lst))

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

(cc 100 us-coins) ; 292

(cc 100 (my-reverse us-coins)) ; order does not matter

(cc 100 uk-coins) ; 104561

(cc 100 (my-reverse uk-coins)) ; order does not matter


#|
Exercise 2.20
|#

(define (same-parity int . lst)
  (let ([parity (if (odd? int) odd? even?)]
        [memo (cons int lst)])
    (filter parity memo)))

(same-parity 1 2 3 4) ; (1 3)

(same-parity 2 3 4 5) ; (2 4)

