#lang racket
;----------------ABSTRACTIZARE LA NIVEL DE PROCES----------------
(define (sum-interval a b)
  (if (> a b)
      0
      (+ a (sum-interval (add1 a) b))))
;(sum-interval 1 9)

(define (aprox-e lim)
  (define (iter i)
    (if (> i lim)
        0
        (+ (/ 1. (fact i)) (iter (add1 i)))))
  (iter 0))

(define (aprox-pi2/8 lim)
  (define (iter i)
    (if (> i lim)
        0
        (+ (/ 1. (sqr i)) (iter (+ i 2)))))
  (iter 1))

(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))

;(aprox-e 10)
;(aprox-e 5)

;(sqrt (* 8 (aprox-pi2/8 1000000)))

(define (series-sum start stop next-term f)
  (if (> start stop)
      0
      (+ (f start)
         (series-sum (next-term start) stop next-term f))))

(define (ex1 a b) (series-sum a b add1 (λ (x) x)))
;(ex1 1 9)
(define (ex2 lim) (series-sum 0 lim add1 (λ (x) (/ 1. (fact x)))))
;(ex2 10)
(define (ex3 lim) (series-sum 1 lim (λ (x) (+ x 2)) (λ (x) (/ 1. (sqr x)))))
;(sqrt (* 8 (ex3 1000000)))

;----------------FUNCTIONALE------------------
(define (initials names)
  (if (null? names)
      '()
      (cons (substring (car names) 0 1)
            (initials (cdr names)))))
;(initials '("Winston" "Leonard" "Spencer" "Churchill"))
;(map (λ (x) (substring x 0 1)) '("Winston" "Leonard" "Spencer" "Churchill"))

(define (squares numbers)
  (if (null? numbers)
      '()
      (cons (sqr (car numbers))
            (squares (cdr numbers)))))
;(squares (range 2 7))
;(map sqr (range 2 7))

(define (pronouns words)
  (cond ((null? words) '())
        ((member (car words) '(I you he she we they)) (cons (car words) (pronouns (cdr words))))
        (else (pronouns (cdr words)))))
;(pronouns '(I think you are who she said you are))
;(filter (λ (x) (member x '(I you he she we they))) '(I think you are who she said you are))

;(map random (range 2 7))

;nr de elem pare dintr-o lista
;(foldr (λ (x acc) (if (even? x) (add1 acc) acc))
;       0
;       (range 2 10))

(define (my-map f L)
  (foldr (λ (x acc) (cons (f x) acc))
         '()
         L))
;(my-map sqr (range 2 7))
(define (my-filter p L)
  (foldr (λ (x acc) (if (p x) (cons x acc) acc))
         '()
         L))
;(my-filter even? (range 2 12))


;----------------ABSTRACTIZARE LA NIVEL DE DATE------------------        
(define make-complex cons)
(define real car)
(define imag cdr)

(define (add-c C1 C2)
  (make-complex
   (+ (real C1) (real C2))
   (+ (imag C1) (imag C2))))

;(add-c (make-complex -1 2) (make-complex 4 1))

;Constructori
;        empty-bst : -> BST
(define empty-bst '())
;        make-bst : BST x Elem x BST -> BST
(define make-bst list)
;Operatori
;        left : BST -> BST
(define left first)
;        right : BST -> BST
(define right third)
;        key : BST -> Elem
(define key second)
;        empty-bst? : BST -> Bool
(define empty-bst? null?)
;        insert-bst : Elem x BST -> BST
(define (insert-bst x tree)
  (cond ((empty-bst? tree) (make-bst empty-bst x empty-bst))
        ((< x (key tree)) (make-bst (insert-bst x (left tree))
                                    (key tree)
                                    (right tree)))
        (else (make-bst (left tree)
                        (key tree)
                        (insert-bst x (right tree))))))
;        list->bst : List -> BST
(define (list->bst L)
  (foldl insert-bst empty-bst L))

'((() 1 (() 2 ())) 3 ((() 4 ()) 6 (() 8 ())))

(list->bst '(3 6 1 2 4 8))
