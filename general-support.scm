#lang racket
(require math)

; on vector
(define (vector-first a-vector)
  (vector-ref a-vector 0))
(define (vector-second a-vector)
  (vector-ref a-vector 1))
(define (vector-third a-vector)
  (vector-ref a-vector 2))
(define (vector-last a-vector)
  (vector-ref a-vector (- (vector-length a-vector) 1)))
(define (vector-rest a-vector)
  (vector-drop a-vector 1))

; on random moves
(define (random+ n)
  (add1 (random (- n 1))))
(define (random+1 n)
  (add1 (random n)))
(define (random-mem a-list)
  (list-ref a-list (random (length a-list))))

; symbol -> color
(define (sym->color sym)
  (cond [(equal? 'x sym) "green"]
	[else "red"]))

; accumulated sum
(define (accum a-list)
  (for/list ([n (length a-list)])
	    (sum (take a-list (+ n 1)))))

; flatten a nested vector into a list
(define (flatten->list a-nested-vector)
  (flatten
   (map vector->list (vector->list a-nested-vector))))

; round off a number to some decimal
(define (round-off number decimal)
  (let ([round-multiplier (expt 10 decimal)])
    (/ (round (* number round-multiplier))
       round-multiplier)))
; source: ...

; shuffle a vector
(define (shuffle x)
  (do ((v (list->vector x)) (n (length x) (- n 1)))
      ((zero? n) (vector->list v))
    (let* ((r (random n)) (t (vector-ref v r)))
      (vector-set! v r (vector-ref v (- n 1)))
      (vector-set! v (- n 1) t))))
; source: ...


(provide vector-first
	vector-second
	vector-third
	vector-last
	vector-rest
	random+
	random+1
	random-mem
	sym->color
	accum
	flatten->list
	round-off
	
	)
