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


#|
Exercise 2.27
|#

(define (deep-reverse l)

  (letrec ([iter (lambda (inner-l memo)
                   (cond
                     ((null? inner-l) memo)
                     ((list? (car inner-l))
                       (iter (cdr inner-l) (cons (iter (car inner-l) (list)) memo)))
                     (else
                       (iter (cdr inner-l) (cons (car inner-l) memo)))))])
    (iter l (list))))

(deep-reverse '(1 2 3)) ; (3 2 1)
(deep-reverse '(1 2 3 (4 5 6))) ; ((6 5 4) 3 2 1)
(deep-reverse '((1 2) ((3 4) (5 6 7)))) ; (((7 6 5) (4 3)) (2 1))


#|
Exercise 2.28
|#

; inspired by https://kelvinh.github.io/wiki/sicp/#sec-2-28

(define (fringe items)
  (letrec ([iter (lambda (items result)
                   (cond ((null? items) result)
                         ((not (pair? items)) (append result (list items)))
                         (else (iter (cdr items) (iter (car items) result)))))])
    (iter items (list))))

(define x (list (list 1 2) (list 3 4)))
(fringe x) ; (1 2 3 4)
(fringe (list x x)) ; (1 2 3 4 1 2 3 4)


#|
Exercise 2.29
|#

; http://www.billthelizard.com/2011/02/sicp-229-binary-mobiles.html

(define (make-mobile left right) (list left right))

(define (make-branch length structure) (list length structure))

(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))

(define (branch-structure branch) (cadr branch))

(define (branch-weight branch)
  (if
    (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

(define (total-weight mobile)
  (+
    (branch-weight (left-branch mobile))
    (branch-weight (right-branch mobile))))

(define (branch-torque branch)
  (*
    (branch-length branch)
    (branch-weight branch)))

(define (branch-balanced? branch)
  (if
    (pair? (branch-structure branch))
      (balanced? (branch-structure branch))
      true))

(define (balanced? mobile)
  (and
    (=
      (branch-torque (left-branch mobile))
      (branch-torque (right-branch mobile)))
    (branch-balanced? (left-branch mobile))
    (branch-balanced? (right-branch mobile))))

(define balanced-mobile (make-mobile (make-branch 2 3) (make-branch 2 3)))

(define unbalanced-mobile (make-mobile (make-branch 2 3) (make-branch 4 5)))

(total-weight balanced-mobile) ; 6

(total-weight unbalanced-mobile) ; 8

(balanced? balanced-mobile) ; #t

(balanced? unbalanced-mobile) ; #f

(define unbalanced-compound-mobile
  (make-mobile
    (make-branch 5 balanced-mobile)
    (make-branch 3 unbalanced-mobile)))

(total-weight unbalanced-compound-mobile) ; 14

(balanced? unbalanced-compound-mobile) ; #f

(define balanced-compound-mobile
  (make-mobile
    (make-branch 10 balanced-mobile)
    (make-branch 12 5)))

(total-weight balanced-compound-mobile) ; 11

(balanced? ) ; #t

(define (new-make-mobile left right) (cons left right))

(define (new-make-branch length structure) (cons length structure))

(define (new-right-branch mobile) (cdr mobile))

(define (new-branch-structure branch) (cdr branch))

(make-mobile (make-branch 2 3) (make-branch 2 3)) ; ((2 3) (2 3))

(new-make-mobile (new-make-branch 2 3) (new-make-branch 2 3)) ; ((2 . 3) 2 . 3)


#|
Exercise 2.30
|#

(define sample-list
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(define (square-tree-direct tree)
  (let ([square (lambda (n) (* n n))])
  (cond
    ((null? tree) '())
    ((not (pair? tree)) (square tree))
    (else
      (cons
        (square-tree-direct (car tree))
        (square-tree-direct (cdr tree)))))))

(square-tree-direct sample-list) ; '(1 (4 (9 16) 25) (36 49))

(define (square-tree-ho tree)
  (let ([square (lambda (n) (* n n))])
    (map
      (lambda (subtree)
        (if (not (pair? subtree))
          (square subtree)
          (square-tree-ho subtree)))
      tree)))

(square-tree-ho sample-list) ; '(1 (4 (9 16) 25) (36 49))


#|
Exercise 2.31
|#

(define (tree-map func tree)
  (map
    (lambda (subtree)
      (if (not (pair? subtree))
        (func subtree)
        (tree-map func subtree)))
    tree))

(tree-map (lambda (n) (* n n)) sample-list) ; '(1 (4 (9 16) 25) (36 49))


#|
Exercise 2.32
|#

(define (subsets s)
  (if (null? s)
    (list s)
    (let ([head (car s)]
          [tail (subsets (cdr s))])
      (append
        tail
        (map (lambda (x) (cons head x)) tail)))))

      ; (append '(() (2)) '((1) (1 2))) == (() (2) (1) (1 2))

(define subset-list (list 1 2 3))

(subsets subset-list) ; '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))


#|
Exercise 2.33
|#

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (my-map proc lst)
  (accumulate
    (lambda (x y) (cons (proc x) y))
    '()
    lst))

(my-map (lambda (x) (* x x)) (list 1 2 3)) ; (1 4 9)

(define (my-append lst1 lst2)
  (accumulate
    cons
    lst2
    lst1))

(my-append '(1 2) '(3)) ; (1 2 3)

(define (my-length lst)
  (accumulate
    (lambda (_ x) (+ x 1))
    0
    lst))

(my-length (list 1 2 3 4)) ; 4


#|
Exercise 2.34
|#

; http://www.billthelizard.com/2011/04/sicp-234-horners-rule.html

(define (horner-eval x coefficient-sequence)
  (accumulate
    (lambda (this-coeff higher-terms)
      (+ (* x higher-terms) this-coeff))
    0
    coefficient-sequence))

(horner-eval 2 (list 1 3 0 5)) ; 47


#|
Exercise 2.35
|#

; http://www.billthelizard.com/2011/04/sicp-235-counting-leaves-of-tree.html

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (accum-count-leaves tree)
  (let (
        [flattened-tree (enumerate-tree tree)]
        [const-1 (lambda (x) 1)])
    (accumulate
      +
      0
      (map
        const-1
        flattened-tree))))

(accum-count-leaves (list 1 (list 1 2 3))) ; 4


#|
Exercise 2.36
|#

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) ; (22 26 30)


#|
Exercise 2.37
|#

; http://www.billthelizard.com/2011/04/sicp-236-237-matrix-algebra.html

(define (dot-product v w) (accumulate + 0 (map * v w)))

(dot-product '(1 2 3) '(4 5 6)) ; 32

(define (matrix-*-vector m v) (map (lambda (row) (dot-product row v)) m))

(matrix-*-vector '((1 2 3) (4 5 6) (7 8 9)) '(10 10 10)) ; (60 150 240)

(define (transpose mat) (accumulate-n cons '() mat))

(transpose '((1  2  3)
             (4  5  6)
             (7  8  9)
             (10 11 12)))

          ; ((1 4 7 10)
          ;  (2 5 8 11)
          ;  (3 6 9 12))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define a '((14 9 3) (2 11 15) (0 12 17) (5 2 3)))

(define b '((12 25) (9 10) (8 5)))

(matrix-*-matrix a b) ; ((273 455) (243 235) (244 205) (102 160))


#|
Exercise 2.38
|#

; http://www.billthelizard.com/2011/04/sicp-238-239-folding-left-and-right.html

(define (fold-right op initial sequence)
   (if (null? sequence)
       initial
       (op (car sequence)
           (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
   (define (iter result rest)
     (if (null? rest)
         result
         (iter (op result (car rest))
               (cdr rest))))
   (iter initial sequence))

(fold-right / 1 (list 1 2 3)) ; 3/2

(fold-left / 1 (list 1 2 3)) ; 1/6

(fold-right list null (list 1 2 3)) ; (1 (2 (3 ())))

(fold-left list null (list 1 2 3)) ; (((() 1) 2) 3)

; Commutativity and associativity assure that foldl and foldr will produce the
; same output.

; Commutativity
; A + B = B + A

; Associativity
; (2 + 3) + 4 == 2 + (3 + 4)


#|
Exercise 2.39
|#

(define (foldr-reverse sequence)
  (fold-right
    (lambda (x y) (append y (list x)))
    '()
    sequence))

(foldr-reverse '(1 2 3)) ; (3 2 1)

(define (foldl-reverse sequence)
  (fold-left
    (lambda (x y) (cons y x))
    '()
    sequence))

(foldl-reverse '(1 2 3)) ; (3 2 1)


#|
Exercise 2.40
|#

; Given a positive integer n, find all ordered pairs of distinct positive
; integers i and j, where 1 <= j < i <= n, such that (i + j) is prime.

; http://www.billthelizard.com/2011/05/sicp-240-241-nested-mappings.html

(enumerate-interval 1 4) ; (1 2 3 4)

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

(require math/number-theory)

(define (flatmap proc seq)
     (accumulate append null (map proc seq)))

; ((1) (1 4) (1 4 9) (1 4 9 16))
; (map
;   (lambda (n)
;     (map
;       (lambda (nn) (* nn nn))
;       (enumerate-interval 1 n)))
;   (list 1 2 3 4))

; (1 1 4 1 4 9 1 4 9 16)
; (flatmap
;   (lambda (n)
;     (map
;       (lambda (nn) (* nn nn))
;       (enumerate-interval 1 n)))
;   (list 1 2 3 4))

(define (prime-sum? pair)
     (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
     (append
       (list (car pair))
       (list (cadr pair))
       (list (+ (car pair) (cadr pair)))))

(define (prime-sum-pairs n)
  (map
    make-pair-sum
    (filter
      prime-sum?
      (flatmap
        (lambda (i)
          (map
            (lambda (j) (list i j))
            (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))))

(prime-sum-pairs 6) ; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))


#|
Exercise 2.41
|#

; write a procedure to find all ordered triples of distinct positive integers
; i, j, and k less than or equal to a given integer n that sum to a given
; integer s.

(define (ordered-triples n)
   (flatmap (lambda (i)
      (flatmap (lambda (j)
         (map (lambda (k)
                (list i j k))
              (enumerate-interval 1 (- j 1))))
       (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(ordered-triples 4) ; ((3 2 1) (4 2 1) (4 3 1) (4 3 2))

(define (ordered-triples-for-sum-s n s)
  (let ([triples (ordered-triples n)])
    (filter
      (lambda (triple)
        (match-let
          ([(list first second third) triple])
          (= s (foldl + 0 (list first second third)))))
      triples)))

(ordered-triples-for-sum-s 100 10) ; ((5 3 2) (5 4 1) (6 3 1) (7 2 1))


#|
Exercise 2.42
|#

(define (make-position row col)
   (cons row col))

(define (position-row position)
   (car position))

(define (position-col position)
   (cdr position))

(define empty-board null)

(define (adjoin-position row col positions)
   (append positions (list (make-position row col))))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (safe? col positions)
   (let ((kth-queen (list-ref positions (- col 1)))
         (other-queens (filter (lambda (q)
                                 (not (= col (position-col q))))
                               positions)))
   (define (attacks? q1 q2)
     (or (= (position-row q1) (position-row q2))
         (= (abs (- (position-row q1) (position-row q2)))
            (abs (- (position-col q1) (position-col q2))))))

   (define (iter q board)
     (or (null? board)
         (and (not (attacks? q (car board)))
              (iter q (cdr board)))))
   (iter kth-queen other-queens)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 4)


#|
Exercise 2.43
|#

; http://www.billthelizard.com/2011/06/sicp-242-243-n-queens-problem.html

; why does flipping the nested mapping in flatmap cause this version of the
; program to run more slowly than the original

; (flatmap
;   (lambda (new-row)
;     (map (lambda (rest-of-queens)
;            (adjoin-position new-row k rest-of-queens))
;          (queen-cols (- k 1))))
;   (enumerate-interval 1 board-size))

; In the original solution, queen-cols is called once for each column in the
; board. This is an expensive procedure to call, since it generates the sequence
; of all possible ways to place k queens in k columns. By moving queen-cols so it
; gets called by flatmap, we're transforming a linear recursive process to a
; tree-recursive process. The flatmap procedure is called for each row of the kth
; column, so the new procedure is generating all the possible solutions for the
; first k - 1 columns for each one of these rows.

 ; If it takes time T to execute the original version of queens for a given
 ; board size, we can expect the new version to take roughly Tboard-size time
 ; to execute.

#|
Exercise 2.44
|#

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))


#|
Exercise 2.45
|#

(define (split x y)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller ((split x y) painter (- n 1))))
        (x painter (y smaller smaller))))))

(define right-split (split beside below))

; (paint (right-split einstein 6))

(define up-split (split below beside))

; (paint (up-split einstein 6))


#|
Exercise 2.46
|#

; http://www.billthelizard.com/2011/09/sicp-246-248-frames-painters.html

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect u v)
  (make-vect
    (+ (xcor-vect u) (xcor-vect v))
    (+ (ycor-vect u) (ycor-vect v))))

(define (sub-vect u v)
  (make-vect
    (- (xcor-vect u) (xcor-vect v))
    (- (ycor-vect u) (ycor-vect v))))

(define (scale-vect s v)
  (make-vect
    (* s (xcor-vect v))
    (* s (ycor-vect v))))

(define u (make-vect 2 4))    ; (2 . 4)
(define v (make-vect 3 1))    ; (3 . 1)
(define w (add-vect u v))     ; (5 . 5)
(define x (sub-vect w u))     ; (3 . 1)
(define y (scale-vect 3 w))   ; (15 . 15)


#|
Exercise 2.47
|#

(define (list-make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (list-origin-frame frame) (car frame))

(define (list-edge1-frame frame) (cadr frame))

(define (list-edge2-frame frame) (caddr frame))

; ((0 . 0) (2 . 4) (3 . 1))
(define list-frame (list-make-frame (make-vect 0 0) (make-vect 2 4) (make-vect 3 1)))

(list-origin-frame list-frame) ; (0 .0)

(list-edge1-frame list-frame) ; (2 . 4)

(list-edge2-frame list-frame) ; (3 . 1)


(define (cons-make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (cons-origin-frame frame) (car frame))

(define (cons-edge1-frame frame) (cadr frame))

(define (cons-edge2-frame frame) (cddr frame))

; ((0 . 0) (2 . 4) 3 .1)
(define cons-frame (foo (make-vect 0 0) (make-vect 2 4) (make-vect 3 1)))

(cons-origin-frame cons-frame) ; (0 . 0)

(cons-edge1-frame cons-frame) ; (2 . 4)

(cons-edge2-frame cons-frame) ; (3 . 1)


#|
Exercise 2.48
|#

(define (make-segment v1 v2) (cons v1 v2))

(define (start-segment segment) (car segment))

(define (end-segment segment) (cdr segment))

(define seg (make-segment (make-vect 0 0) (make-vect 1 1))) ; ((0 . 0) 1 . 1)

(start-segment seg) ; (0 . 0)

(end-segment seg) ; (1 . 1)
