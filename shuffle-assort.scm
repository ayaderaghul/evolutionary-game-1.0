(require "specialised-support.scm")

(define (shuffle-world a-world)
 (pack-a-world
  (list->vector
   (shuffle
    (flatten->list a-world)))))

(define (shuffle! world)
  (let ([shuffled (shuffle-world world)])
   (for* ([i row]
	   [j col])
	  (vector-set! (vector-ref world i) j
		       (vector-ref
			(vector-ref shuffled i) j)))))

(define (assort! world)
  (let* ([auto-world (list->vector (flatten->list world))]
         [n (sub1 (/ (vector-length auto-world) 2))])
    (for ([k n])
	 (let*
	     ([i (floor (/ (add1 (* k 2)) 10))]
	      [j (- (add1 (* k 2)) (* i 10))])
	   (when (< (random) assortment)
		 (vector-set! (vector-ref world i) j
			      (clone
			       (vector-ref (vector-ref world i)
					   (sub1 j)))))))))
