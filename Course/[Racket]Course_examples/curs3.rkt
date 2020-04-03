#lang racket
(define (doi-zece L)
  (doi-zece-helper L '()))

(define (doi-zece-helper L acc)
  (if (null? L)
      (reverse acc)
      (if (and (>= (car L) 2)
               (<= (car L) 10))
          (doi-zece-helper (cdr L) (cons (car L) acc))
          (doi-zece-helper (cdr L) acc))))

;(doi-zece '(2 5 1 6 3 2 8 11 13 9 10))


(define fs (list + (λ (x y) (- x y y)) 5))
;fs
;((cadr fs) 20 8)


(define (ins-sort op)
  (λ (L)
    (if (null? L)
        L
        (ins op (car L) ((ins-sort op) (cdr L))))))

(define (ins op x L)
  (cond ((null? L) (list x))
        ((op x (car L)) (cons x L))
        (else (cons (car L) (ins op x (cdr L))))))

(define sort< (ins-sort <))
(define sort> (ins-sort >))

;(sort< '(6 1 3 7 3 7 0 5 4 21 6))
;(sort> '(6 1 3 7 3 7 0 5 4 21 6))

;(define (transform-evens f L)
;  (cond ((null? L) L)
;        ((even? (car L)) (cons (f (car L)) (transform-evens f (cdr L))))
;        (else (transform-evens f (cdr L)))))
;
;(define (transform-odds f L)
;  (cond ((null? L) L)
;        ((odd? (car L)) (cons (f (car L)) (transform-odds f (cdr L))))
;        (else (transform-odds f (cdr L)))))

(define (transform pred?)
  (λ (f L)
    (cond ((null? L) L)
          ((pred? (car L)) (cons (f (car L)) ((transform pred?) f (cdr L))))
          (else ((transform pred?) f (cdr L))))))

(define transform-evens (transform even?))
(define transform-odds (transform odd?))

(transform-evens add1 '(1 2 4 7 10 5))
(transform-odds add1 '(1 2 4 7 10 5))