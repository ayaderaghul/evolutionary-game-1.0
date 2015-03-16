#lang racket
;; RUN

(define (trial-run steps delay)
  (for/and ([n steps])
         ;  (send payoff-series-dc erase)
         ;  (sleep delay)
           (match-on! A A-pay)
           (draw-world A)
           (draw-pie A)
           (plot-series n)
           (plot-payoff-space A-pay)
           (abridged-report!)
           (sleep delay)
           (regenerate! A A-fit)))

(define (evolve-world! world steps delay)
  (for/and ([n steps])
	   (send dc3 erase)
	 ;  (draw-world world)
	   (sleep delay)
	   (match-on! world A-pay)
	   (do-cal! A-pay A+ A-sum A-share)
	   (sleep delay)
	   (let ([B (ghost-book row col)])
	     (scan! A B)
	     (regenerate! A B A-share)
	     (sleep delay))
	   ;(for ([i 1])
		(mutate! world)
	;	)
	   (sleep delay)
	   (fix! world)
	   (sleep delay)
	   (shuffle! world)
	   (sleep delay)
	   (assort! A)
	   (sleep delay)
	   (plot-payoff A-pay)
	   (sleep delay)
	 ;  (parameterize ([plot-x-transform log-transform])
			 (plot-series n)
	;		 )
	  (sleep delay)
	  (draw-pie A)
	  ; (sleep 0.1)))
	   ;(export-data "A-pay.txt" A-pay)
	   ;(export-data "A.txt" A)))
	   ))
`(let ([B (ghost-book row col)])
   (scan! A B)
   (regenerate! A B)
   B)


(new button% [parent frame1]
     [label (format "Next ~a turns" one-run)]
     ;; Callback for a button click:
     [callback (lambda (button event)
                 (evolve-world! A   ;;
                                one-run pause))])

(provide trial-run)
