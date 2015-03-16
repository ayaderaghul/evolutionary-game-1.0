(require "general-support.scm")

(define 2-pure-strats (list 'x 'y))

(define (mutate-color! world)
  (let* ([a-row (vector-ref world (random row))]
	 [a-col (random col)]
	 [an-auto (vector-ref a-row a-col)]
	 [r (random+ (vector-length an-auto))]
	 [a-state (vector-ref an-auto r)]
	 [a-color (vector-ref a-state 2)])
    (vector-set! a-state 2
		 (random-mem (remove a-color 2-pure-strats)))))

(define (mutate-traject! world)
  (let* ([a-row (vector-ref world (random row))]
	 [a-col (random col)]
	 [an-auto (vector-ref a-row a-col)]
	 [r (random+ (vector-length an-auto))]
	 [a-state (vector-ref an-auto r)])
    (vector-set! a-state (random 2)
		 (random+ (vector-length an-auto)))))

(define (mutate-add! world)
  (let* ([a-row (vector-ref world (random row))]
	 [a-col (random col)]
	 [an-auto (vector-ref a-row a-col)]
	 [l (vector-length an-auto)]
	 [r (random+ l)]
	 [a-state (vector-ref an-auto r)])
    (vector-set! a-row a-col
		 (vector-append an-auto
				(vector
				 (vector
				  (random+1 l)
				  (random+1 l)
				  (random-mem 2-pure-strats)))))
    (vector-set! a-state (random 2) l)))

(define (mutate-del! world)
  (let* ([a-row (vector-ref world (random row))]
	 [a-col (random col)]
	 [an-auto (vector-ref a-row a-col)]
	 [l (vector-length an-auto)]
	 [r (random+ l)])
    (when (> l 2)
	  (vector-set! a-row a-col
		       (vector-append
			(vector-take an-auto r)
			(vector-drop an-auto (add1 r)))))))

(define (mutate! world)
  (let ([a (random)])
    (when
     (< a mutation-rate)
     (let ([b (random 4)])
       (cond [(equal? b 0) (mutate-color! world)]
	     [(equal? b 1) (mutate-traject! world)]
	     [(equal? b 2) (mutate-add! world)]
	     [else (mutate-del! world)])))))
; check trajectory

(define (check-traject! auto)
  (let ([l1 (vector-length auto)])
    (for* ([i (in-range 1 l1)]
	   [j 2])
	  (when (> (vector-ref (vector-ref auto i) j) (- l1 1))
		(vector-set!
		 (vector-ref auto i) j (random+ l1))))))

(define (fix-traject! world)
  (for* ([i row]
	 [j col])
	(check-traject! (vector-ref (vector-ref world i) j))))


(define (reset-state! world)
  (for* ([i row]
	 [j col])
	(vector-set! (vector-ref (vector-ref world i) j)
		     0 1)))

(define (fix! world)
  (for/and ([n 1])
	   (fix-traject! world)
	   (reset-state! world)))

