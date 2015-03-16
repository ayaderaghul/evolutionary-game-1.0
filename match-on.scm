(require "general-support.scm")
(require math)


;; Match on! matrix
;     C      D
; C  3 3    0 4
; D  4 0   -1 -1

(define (match-strat a b)
  (if (equal? a 'x)
      (if (equal? b 'x) (list 3 3)
	  (list 0 4))
      (if (equal? b 'y) (list -1 -1)
	  (list 4 0))))

;; payoff calculation
(define (mean-pay posn pay-list)      ; discount factor = 1/rounds
  (mean
   (for/list ([i (length pay-list)])
	     (posn (list-ref pay-list i)))))

(define (utilize payoff)     ; wtf, ah, non EUT
  (+ (* 2 (expt payoff 3))
     (- (* 3 (expt payoff 2)))
     (* 2 payoff)))

(define (discount-stream posn pay-list)
  (round-off
   (sum
    (for/list ([i (length pay-list)])
	      (* (expt discount-factor i)
                 (posn (list-ref pay-list i))))) 2)
  )

(define (posn? opponent-strat)
  (cond [(equal? opponent-strat 'x) vector-first]
	[else vector-second]))

(define (after-complex-cost a-payoff length-of-auto)
  (- a-payoff (* complex-cost-rate length-of-auto)))

(define (minus-complex-cost payoff-list length-list)
  (list
   (after-complex-cost (first payoff-list) (first length-list))
   (after-complex-cost (last payoff-list) (last length-list))))


(define (match-auto! world i a b) ; i = row index
  (let ([pay-list
	 (for/list ([n repeated-times])
		   (let* ([auto1 (vector-ref (vector-ref world i) a)]
			  [auto2 (vector-ref (vector-ref world i) b)]
			  [l1 (- (vector-length auto1) 1)]
			  [l2 (- (vector-length auto2) 1)]
			  [n1 (vector-first auto1)]
			  [n2 (vector-first auto2)]
			  [current-state1 (vector-ref auto1 n1)]
			  [current-state2 (vector-ref auto2 n2)]
			  [current-strat1 (vector-last current-state1)]
			  [current-strat2 (vector-last current-state2)])
		     (vector-set! auto1 0
				  ((posn? current-strat2) current-state1))
		     (vector-set! auto2 0
				  ((posn? current-strat1) current-state2))
		     (minus-complex-cost
                      (match-strat current-strat1 current-strat2) (list l1 l2))
		     ))])
    pay-list))

(define (set-payoff! world i a b pay-book)
  (let ([payoff (match-auto! world i a b)])
    (vector-set! (vector-ref pay-book i) a
		 (discount-stream first payoff))
    (vector-set! (vector-ref pay-book i) b
		 (discount-stream last payoff))))

(define even-list
  (filter even? (build-list col values)))

(define (match-row! world i pay-book)  ; i = row index
  (for ([k even-list])
       (set-payoff! world
		    i
		    k (+ k 1) pay-book)))

(define (match-on! world pay-book)
  (for ([j row])
       (match-row! world j pay-book)))
