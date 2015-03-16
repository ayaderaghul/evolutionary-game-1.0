#lang racket

(require racket/gui/base ; GUI utilities (to have TV)
         plot/no-gui ; to plot graphs & update plot
         racket/draw ; to draw pixels
         racket/math ; to draw arc in pie chart
         math) ; to have some maths algorithm (eg: mean, sum..)
(require "general-support.scm")

;; for matrix tab
(define (delete-a-child a-pane a-child)
  (send a-pane delete-child a-child))
(define (delete-children a-pane a-child-list)
  (map (lambda (x) (delete-a-child a-pane x)) a-child-list))
(define (delete-all-children a-pane)
  (delete-children a-pane (send a-pane get-children)))

(define (add-a-child a-pane a-child)
  (send a-pane add-child a-child))
(define (add-children a-pane a-child-list)
  (map (lambda (x) (add-a-child a-pane x))
       a-child-list))

;; draw payoff space
(define (vector-flatten a-nested-vector)
  (list->vector
   (flatten (map
             vector->list
              (vector->list a-nested-vector)))))
(define (pack a-vector)
  (for/list ([i (/ (vector-length a-vector) 2)])
            (vector-take (vector-drop a-vector (* i 2)) 2)))
(define (pack-a-world a-vector)
  (for/vector ([i 10])
              (vector-take (vector-drop a-vector (* i 10)) 10)))



;; automata
(define (morph an-auto)
  (let ([listed (vector->list an-auto)])
    (append (take listed 1)
	    (map vector->list
		 (rest listed)))))

(define (clone an-auto)
  (let* ([morphed (morph an-auto)]
	 [vectored (list->vector morphed)])
    (vector-append (vector-take vectored 1)
		   (vector-map list->vector
			       (vector-rest vectored)))))

;; draw fitness
(define (convert->string a)
  (if (number? a) (number->string a)
      (symbol->string a)))
(define (dna->string an-auto)
  (apply string-append
	 (map convert->string
	      (flatten
	       (rest (morph an-auto))))))
(define (convert->pi a-list)
  (for/list ([n (length a-list)])
	    (* 2 pi (list-ref a-list n))))
(define (dna-color an-auto)
  (let* ([body (rest (morph an-auto))]
	 [n (length body)])
    (for/list ([i n])
	      (last (list-ref body i)))))
(define (portion color a-list)
  (/ (count (lambda (x) (equal? x color)) a-list)
     (length a-list)))
(define (mix-color an-auto)
  (let* ([dna-colors (dna-color an-auto)]
	 [green (exact-round (* 225 (portion 'x dna-colors)))] ; x: cooperate
	 [red (exact-round (* 225 (portion 'y dna-colors)))]
	 )
    (make-color red green 0)))

; calculate the mean of a nested vector
(define (population-mean pay-book)
  (mean (flatten->list pay-book)))

;; shuffle
(define (shuffle x)
  (do ((v (list->vector x)) (n (length x) (- n 1)))
      ((zero? n) (vector->list v))
    (let* ((r (random n)) (t (vector-ref v r)))
      (vector-set! v r (vector-ref v (- n 1)))
      (vector-set! v (- n 1) t))))
; source ...
(define (shuffle-world a-world)
 (pack-a-world
  (list->vector
   (shuffle
    (flatten->list a-world)))))



(provide delete-a-child
	delete-children
	delete-all-children
	add-a-child
	add-children
	vector-flatten
	pack
	pack-a-world
	morph
	clone
	convert->string
	dna->string
	convert->pi
	dna-color
	portion
	mix-color
	population-mean
	shuffle
	shuffle-world
	
	)
