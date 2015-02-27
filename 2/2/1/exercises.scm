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


#|
Exercise 2.21
|#

(define (square-list-wo-map items)
  (if (null? items)
    (list)
    (cons
      (* (car items) (car items))
      (square-list-wo-map (cdr items)))))

(square-list-wo-map (list 1 2 3 4 5)) ; '(1 4 9 16 25)

(define (square-list-w-map items)
  (map
    (lambda (n) (* n n))
    items))

(square-list-w-map (list 1 2 3 4 5)) ; '(1 4 9 16 25)


#|
Exercise 2.22
|#

; http://www.billthelizard.com/2011/01/sicp-221-223-mapping-over-lists.html

(define (square n) (* n n))

; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;       answer
;       (iter (cdr things)
;             ; this cons-es the answer onto the front of the answer list
              ; resulting in a reversed answer list
;             (cons (square (car things))
;                   answer))))
;   (iter items null))

; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;       answer
;       (iter (cdr things)
              ; each cons creates a list containing the previous answer (which
              ; itself is a list) and the new int.
              ; e.g. (cons (list) 1) => (() . 1)
;             (cons answer
;                   (square (car things))))))
;   (iter items null))

; (square-list-w-map (list 1 2 3 4 5)) ; (((((() . 1) . 4) . 9) . 16) . 25)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (append answer
                    (list (square (car things)))))))
  (iter items null))

(square-list (list 1 2 3 4 5)) ; (1 4 9 16 25)


#|
Exercise 2.23
|#

(define (for-each f xs)
  (map f xs)
  #true)

(for-each (lambda (x) (newline) (display x)) (list 1 2 3 4 5))


#|
Exercise 2.24
|#

(list 1 (list 2 (list 3 4)))

; result
; (1 (2 (3 4)))

; box
; [|->][|()]
 ; 1    [|->][|()]
       ; 2    [|->][|()]
             ; 3    4
; tree
; /\
;1 /\
; 2 /\
;  3  4


#|
Exercise 2.25
|#

; (1 3 (5 7) 9)

(car
  (cdr
    (car
      (cdr
        (cdr '(1 3 (5 7) 9)))))) ; 7

; also
(cadar (cddr '(1 3 (5 7) 9))) ; 7


; ((7))
(car
  (car '((7)))) ; 7

; also
(caar '((7))) ; 7


; (1 (2 (3 (4 (5 (6 7))))))

(car
  (cdr
    (car
      (cdr
        (car
          (cdr
            (car
              (cdr
                (car
                  (cdr
                    (car
                      (cdr '(1 (2 (3 (4 (5 (6 7)))))))))))))))))) ; 7

; also

(cadadr
  (cadadr
    (cadadr '(1 (2 (3 (4 (5 (6 7))))))))) ; 7


#|
Exercise 2.26
|#

(define x (list 1 2 3))

(define y (list 4 5 6))

(append x y) ; (1 2 3 4 5 6)

(cons x y) ; ((1 2 3) 4 5 6)

(list x y); ((1 2 3) (4 5 6))
