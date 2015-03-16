(require "general-support.scm")
(require "specialised-support.scm")


(define (regenerate a-world an-id-book individual-fit-book)
  (let ([listed-world (flatten->list a-world)]
	[accum-fit-list (third (abridged-report!))]
	[type-list (first (abridged-report!))])
    (for ([s speed])
	 (let* ([i (random row)]
		[j (random col)]
		[r (random)])
	   (vector-set! (vector-ref a-world i) j
			(clone
			 (list-ref
			  type-list
			  (length
			   (for/list ([n (in-list accum-fit-list)])
				     #:break (not (> r n))
				     n)))))))))

(define (regenerate! a-world individual-fit-book)
  (let ([I (id-book row col)])
    (regenerate a-world I individual-fit-book)))
