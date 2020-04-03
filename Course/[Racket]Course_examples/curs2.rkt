#lang racket
(require (lib "trace.ss"))

(define (fact-stack n)
  (if (zero? n)
      1
      (* n (fact-stack (- n 1)))))

;(trace fact-stack)
;(fact-stack 10)

(define (fact-tail n)
  (fact-tail-helper n 1))

(define (fact-tail-helper n acc)
  (if (zero? n)
      acc
      (fact-tail-helper (- n 1) (* n acc))))

;(trace fact-tail-helper)
;(fact-tail 10)

(define (fibo-stack n)
  (if (< n 2)
      n
      (+ (fibo-stack (- n 1)) (fibo-stack (- n 2)))))

;(trace fibo-stack)
;(fibo-stack 5)
;(fibo-stack 40) ;;  deja incepe sa dureze mult

(define (fibo-tail n)
  (fibo-tail-helper n 0 1))

(define (fibo-tail-helper n a b)
  (if (zero? n)
      a
      (fibo-tail-helper (- n 1) b (+ a b))))

;(trace fibo-tail-helper)
;(fibo-tail 5)
(fibo-tail 100000)

	

