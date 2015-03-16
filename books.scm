(require "general-support.scm")
(require math)



;; pay-book
(define (pay) 0)
(define (pay-row col)
  (for/vector ([j col])
	      (pay)))
(define (pay-book row col)
  (for/vector ([i row])
	      (pay-row col)))

(define A-pay (pay-book row col))

; positive pay book
(define (posi) 0)
(define (posi-book row col)
  (for/vector ([n (* row col)])
	      (posi)))

(define A+ (posi-book row col))

; sum of the posi-book
(define (v-sum) (vector 0)) ; v because this sum is a vector
(define A-sum (v-sum))

; individual fitness book
(define (fit) 0)
(define (fit-book row col)
  (for/vector ([n (* row col)])
	      (fit)))

(define A-fit (fit-book row col))

; id book
(define (id) 0)
(define (id-book row col)
  (for/vector ([n (* row col)])
	      (id)))

`(define B (id-book row col))
; no need to define id book because for each cycle,
; it's better to start out with a blank clear id book
