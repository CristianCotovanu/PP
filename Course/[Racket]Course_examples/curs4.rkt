;; !!! Limbajul trebuie să fie Pretty Big !!!
;---------------EFECTE LATERALE--------------------
;(define counter 1)
;
;(define (display-and-inc-counter)
;  (display counter)
;  (set! counter (add1 counter)))
;
;(display-and-inc-counter)
;(newline)
;(display-and-inc-counter)

;(define a (λ () n))
;(define n 5)
;(a)


;---------------LEGARE DINAMICĂ--------------------
(define (f)
  (g 5))

(define (g x)
  (* x x))
(f)

(define (g x)
  (* x x x))
(f)

;-------------------LAMBDA-------------------------
(define test
  (λ (x y z)
    (display x)
    (if (< y 4)
        (test (add1 y) z z))
    z))

;(test 1 2 4)

;------------------LET-URI-------------------------
;(define x 2) ; să se încerce și cu linia asta comentată
;(let ((x 5) (y (add1 x)))
;  (cons x y))
;
;(let* ((x (add1 x)) (y (add1 x)) (z (+ x y)))
;  (list x y z))
;
;(letrec ((par? (λ (n) 
;                 (if (zero? n)
;                     #t
;                     (impar? (sub1 n)))))
;         (impar? (λ (n)
;                   (if (zero? n)
;                       #f
;                       (par? (sub1 n))))))
;  (par? 243))
;
;(letrec ((x y) (y 1)) x)

(define inversare
  (lambda (nr)
    (let iter ((nr-crt nr) (nr-inv 0))
      (if (zero? nr-crt)
          nr-inv
          (iter (quotient nr-crt 10)
                (+ (* 10 nr-inv)
                   (modulo nr-crt 10)))))))

;(inversare 5123)

;----------------AVANTAJE DEFINE-------------------
;(define par? (λ (n) 
;                 (if (zero? n)
;                     #t
;                     (impar? (sub1 n)))))
;
;(define impar? (λ (n) 
;                 (if (zero? n)
;                     #f
;                     (par? (sub1 n)))))
;
;(impar? 5)

;-------------CONTEXT COMPUTAȚIONAL----------------
;(define a 1)
;(define (f x)
;  (+ x
;     (let ((x 5))
;       (* a x))))
;(f 2)
;(define a 2)
;(f 2)


;(define (fact n)
;  (if (zero? n)
;      1
;      (* n (fact (- n 1)))))
;(define g fact)
;(g 4)
;
;(define (fact n) n)
;(g 4)

;(define (g x)
;  (* x x))
;(define f
;  (g 5))
;f
;
;(define (g x)
;  (* x x x))
;f


