(load "packages.scm")
(require "general-support.scm")

; TEMP: (from payoff matrix)
(define smallest-payoff -1)

;; FROM A-PAY -> POSITIVE PAYBOOK
(define (add2 pay-book posi-book)
  (for* ([i row]
	 [j col])
	(let ([n (+ (* i col) j)])
	  (vector-set! posi-book n
		       (+ (+ 1 (abs smallest-payoff))
			(vector-ref
			 (vector-ref pay-book i) j))))))

;; SUM
(define (do-v-sum posi-book sum-book)
  (vector-set! sum-book 0
	       (sum (flatten (vector->list posi-book)))))

;; INDIVIDUAL FITNESS
(define (calculate-fit-book posi-book sum-book individual-fit-book)
  (for ([n (vector-length individual-fit-book)])
       (vector-set! individual-fit-book n
		    (/ (vector-ref posi-book n) (vector-first sum-book)))))

(define (calculate-fit-book! pay-book posi-book sum-book individual-fit-book)
  (for/and ([n 1])
	   (add2 pay-book posi-book)
	   (do-v-sum posi-book sum-book)
	   (calculate-fit-book posi-book sum-book individual-fit-book)))

; ID BOOK

(define (the-first n a-vector)
  (length (for/list ([i (in-vector a-vector)])
		    #:break (equal? n i) i)))
; it returns the first number n in a vector sequence

;; THE IDEA OF AN ID BOOK
; ID book is a book of all 0
; the first agent will have id 0
; after the first agent, the first of another kind will have id 1
; hence in one scan, the world will be separated into 0 and 1
; the first kind has id 0, the rest (no matter how many kinds) has id 1
; now the first kind is already done
; we focus on the smaller set of "the rest" which has id 1
; we do it again, the first agent in that subset will have id 1
; the first of another kind will have id 2
; hence in this scan, we can separate the subworld into kind 1 and the rest
; the rest will have id 2 for now
; and so on

; this mechanism is the situation that you dont know
; how many loops you have to do
; so you use `loop with `when
; it's more complicated and not as neat as `for

(define (twin-helper object n a-list a-vector)        ; the benchmark object has id n (as base id)
  (for ([i (length a-list)])                          ; scan the whole world
       (and (> (vector-ref a-vector i) (sub1 n))      ; we are going up on the id stair, so dont scan the agents having id < n
	    (not (equal? object (list-ref a-list i))) ; if the agent = the object, leave id n
	    (vector-set! a-vector i (add1 n)))))      ; otherwise, set id of the agent n+1

; here we make the above function neater
(define (twin a-base-id a-list a-vector)              ; given an id, automatically find the benchmark object
  (twin-helper (list-ref a-list (the-first a-base-id a-vector)) ; which is the first agent having that id in the world
	       a-base-id
	       a-list
	       a-vector))

(define (scan an-object-list an-id-book)    ; given an object list and an id book
  (let ([a-base-id 0])                      ; set the base id at 0 so we can go up from 0
    (let loop ()
      (twin a-base-id an-object-list an-id-book) ; scan the list for the twin of the agent id 0
      (set! a-base-id (add1 a-base-id))          ; increase the base id 1 unit
      (when (< (the-first a-base-id an-id-book)  ; loop the scan until
               (vector-length an-id-book))       ; the base id reaches the maximum
            (loop)))))                      ; ie: a world of 10 agents cant have type 11
; source..

;; TEMP (as example)
(define AA (vector (vector 'a) (vector 'b) (vector 'a))) ; a world
(define BB (vector 0 0 0)) ; an id book
(define CC (vector 1/2 1/4 1/4)) ; an individual payshare book


(define (scan! a-world an-id-book)
  (let ([listed-world (flatten->list a-world)])
    (scan listed-world an-id-book)))

;; GIVEN THE ID BOOK -> POPULATION SEGMENTATION
(define (how-many-types an-id-book)
  (+ 1 (apply max (vector->list an-id-book))))

(define (what-types a-world an-id-book)
  (let ([listed-world (flatten->list a-world)])
    (for/list ([i (in-list
                   (for/list ([a (how-many-types an-id-book)])
                             (the-first a an-id-book)))])
              (list-ref listed-world i))))

(define (type-stats an-id-book)
  (let ([id-list (vector->list an-id-book)])
    (for/list ([t (how-many-types an-id-book)])
	      (count (lambda (x) (equal? x t)) id-list))))

(define (type-share an-id-book)
  (let* ([demo-list (type-stats an-id-book)]
	 [total (sum demo-list)])
    (for/list ([i (length demo-list)])
	      (/ (list-ref demo-list i) total))))

;; ADD INDIVIDUAL FITBOOK = POPULATION REPRESENTATION (FITNESS MAP)

(define (type-fitness a-world an-id-book individual-fit-book an-id)
  (let ([indi-fit-list (vector->list individual-fit-book)])
    (sum (filter number?
                 (for/list
                  ([i (length indi-fit-list)])
                  (and (equal? (vector-ref an-id-book i) an-id)
                       (list-ref indi-fit-list i)))))))

(define (type-fitness-list a-world an-id-book individual-fit-book)
  (for/list ([i (how-many-types an-id-book)])
            (type-fitness a-world an-id-book individual-fit-book i)))

;; REPORTING
(define (abridged-report a-world pay-book posi-book sum-book individual-fit-book an-id-book)
  (begin
    (calculate-fit-book! pay-book posi-book sum-book individual-fit-book)
    (scan! a-world an-id-book)
    (list
     (what-types a-world an-id-book)
     (accum (type-share an-id-book))
     (accum (type-fitness-list a-world an-id-book individual-fit-book)))))

(define (abridged-report!)
  (let ([G (id-book row col)])
    (abridged-report A A-pay A+ A-sum A-fit G)))
