(require "specialised-support.scm")


; pure strategies
(define 2-pure-strats (list 'x 'y))
(define 3-pure-strats (list 'x 'y 'z))

; atomic automata

(define (x) (vector 1 (vector 1 1 'x)))
(define (y) (vector 1 (vector 1 1 'y)))
(define (z) (vector 1 (vector 1 1 'z)))

;; 2 pure strategies
;; INITIAL WORLD
(define (create-row col)
  (for/vector ([j col])
	      (let ([r (random)])
		(if (< r init-demo-for-2)
                    (clone (x))
                    (clone (y))))))

(define (create-world row col)
  (for/vector ([i row])
	      (create-row col)))

(define A (create-world row col))
