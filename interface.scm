(load "packages.scm")
(require "general-support.scm"
	"specialised-support.scm")

(define interface (new frame% [label "Primrose"]
                           [width 400]
                           [height 600]))
(define big-frame (new horizontal-pane% [parent interface]))

;; FIRST COLUMN
(define remote-list-pane
  (new vertical-pane% [parent big-frame]
       [min-width 250]))

;; CALLBACK FOR MATRIX TAB
(define (3-strat-matrix)
  (begin
    (add-a-child row-1 r1c3)
    (add-a-child row-2 r2c3)
    (add-children row-3 (list r3c1 r3c2 r3c3))))
(define (2-strat-matrix)
  (begin
    (delete-a-child row-1 r1c3)
    (delete-a-child row-2 r2c3)
    (delete-all-children row-3)))

;; MATRIX TAB
(new message% [parent remote-list-pane] [label "payoff matrix"])
(define matrix-tab
  (new tab-panel%
       [parent remote-list-pane]
       [min-height 150]
       [choices (list "2 strategies"
                      "3 strategies")]
       [callback
        (lambda (b e)
          (if (zero? (send matrix-tab get-selection))
              (begin
                (2-strat-matrix)
                (delete-all-children init-demo-pane)
                (add-a-child init-demo-pane init-demo-for-2))
              (begin
                (3-strat-matrix)
                (delete-all-children init-demo-pane)
                (add-a-child init-demo-pane init-demo-for-3))
                ))]))

(define matrix-pane
  (new vertical-pane% [parent matrix-tab]))

(define row-1
  (new horizontal-pane% [parent matrix-pane]))
(define row-2
  (new horizontal-pane% [parent matrix-pane]))
(define row-3
  (new horizontal-pane% [parent matrix-pane]))

(define r1c1
  (new horizontal-panel% [parent row-1] [style '(border)]))
(define r1c2
  (new horizontal-panel% [parent row-1] [style '(border)]))
(define r1c3
  (new horizontal-panel% [parent row-1] [style '(deleted border)]))
(define r2c1
  (new horizontal-panel% [parent row-2] [style '(border)]))
(define r2c2
  (new horizontal-panel% [parent row-2] [style '(border)]))
(define r2c3
  (new horizontal-panel% [parent row-2] [style '(deleted border)]))
(define r3c1
  (new horizontal-panel% [parent row-3] [style '(deleted border)]))
(define r3c2
  (new horizontal-panel% [parent row-3] [style '(deleted border)]))
(define r3c3
  (new horizontal-panel% [parent row-3] [style '(deleted border)]))

(define r1c1-1
  (new text-field% [parent r1c1] [label ""] [init-value "3"]))
(define r1c1-2
  (new text-field% [parent r1c1] [label ""] [init-value "3"]))
(define r1c2-1
  (new text-field% [parent r1c2] [label ""] [init-value "0"]))
(define r1c2-2
  (new text-field% [parent r1c2] [label ""] [init-value "4"]))
(define r1c3-1
  (new text-field% [parent r1c3] [label ""] [init-value "0"]))
(define r1c3-2
  (new text-field% [parent r1c3] [label ""] [init-value "0"]))

(define r2c1-1
  (new text-field% [parent r2c1] [label ""] [init-value "4"]))
(define r2c1-2
  (new text-field% [parent r2c1] [label ""] [init-value "0"]))
(define r2c2-1
  (new text-field% [parent r2c2] [label ""] [init-value "-1"]))
(define r2c2-2
  (new text-field% [parent r2c2] [label ""] [init-value "-1"]))
(define r2c3-1
  (new text-field% [parent r2c3] [label ""] [init-value "0"]))
(define r2c3-2
  (new text-field% [parent r2c3] [label ""] [init-value "0"]))

(define r3c1-1
  (new text-field% [parent r3c1] [label ""] [init-value "0"] ))
(define r3c1-2
  (new text-field% [parent r3c1] [label ""] [init-value "0"]))
(define r3c2-1
  (new text-field% [parent r3c2] [label ""] [init-value "0"]))
(define r3c2-2
  (new text-field% [parent r3c2] [label ""] [init-value "0"]))
(define r3c3-1
  (new text-field% [parent r3c3] [label ""] [init-value "0"]))
(define r3c3-2
  (new text-field% [parent r3c3] [label ""] [init-value "0"]))

(new message% [parent remote-list-pane]
     [label "initial demographic"])
(define init-demo-pane
  (new vertical-pane%
       [parent remote-list-pane] [min-height 150]))

(define init-demo-for-2 .5)
(define (update-init-demo-for-2!)
  (set! init-demo-for-2 (/ (send init-demo-for-2-slider get-value)
                           100)))
(define init-demo-for-2-slider
  (new slider%
       [parent init-demo-pane]
       [label "strategy 1"]
       [init-value 50]
       [min-value 0]
       [max-value 100]
       [callback (lambda (b e) (update-init-demo-for-2!))]))
(define init-demo-for-3
  (new vertical-panel% [parent init-demo-pane]
       [style '(deleted)]))
(define init-strat-1-slider
  (new slider%
       [parent init-demo-for-3]
       [label "strategy 1"]
       [min-value 0]
       [max-value 100]
       [callback (lambda (b e)
                   (begin
                     (delete-a-child init-demo-for-3 init-strat-2)
                     (set! init-strat-2 (make-init-strat-2))))]))
(define (make-init-strat-2-slider)
  (new slider%
       [parent init-demo-for-3]
       [label "strategy 2"]
       [min-value 0]
       [max-value (- 100 (send init-strat-1-slider get-value))]))
(define init-strat-2-slider
  (make-init-strat-2-slider))

;; CHANNELS & EXPORT

(new message% [parent remote-list-pane] [label "choose channels? and export?"])
(define population-shelf
  (new horizontal-pane% [parent remote-list-pane]
       [stretchable-height #t]))

(new message% [parent population-shelf]
     [label "population"])

(define population-channel #f)
(define (update-population-channel!)
  (set! population-channel (send population-channel-box get-value)))
(define population-channel-box
  (new check-box%
       [label ""]
       [parent population-shelf]))

(define population-export #f)
(define population-export-box
  (new check-box%
       [label ""]
       [parent population-shelf]))

(define starting-strat-shelf
  (new horizontal-pane% [parent remote-list-pane]))
(new message% [parent starting-strat-shelf] [label "starting strategy"])
(define starting-strat-channel #f)
(define starting-strat-channel-box
  (new check-box%
       [label ""] [parent starting-strat-shelf]))
(define starting-strat-export #f)
(define starting-strat-export-box
  (new check-box%
       [label ""] [parent starting-strat-shelf]))

(define payoff-space-shelf
  (new horizontal-pane% [parent remote-list-pane]
       [stretchable-height #t]))
(new message% [parent payoff-space-shelf] [label "payoff space"])
(define payoff-space-channel #f)
(define payoff-space-channel-box
  (new check-box%
       [label ""]
       [parent payoff-space-shelf]))
(define payoff-space-export #f)
(define payoff-space-export-box
  (new check-box%
       [label ""]
       [parent payoff-space-shelf]))

(define payoff-series-shelf
  (new horizontal-pane% [parent remote-list-pane]))
(new message% [parent payoff-series-shelf] [label "payoff series"])
(define payoff-series-channel #t)
(define payoff-series-channel-box
  (new check-box%
       [label ""] [value #t]
       [parent payoff-series-shelf]))
(define payoff-series-export #f)
(define payoff-series-export-box
  (new check-box%
       [label ""]
       [parent payoff-series-shelf]))

(define fitness-shelf
  (new horizontal-pane% [parent remote-list-pane]))
(new message% [parent fitness-shelf] [label "fitness"])
(define fitness-channel #t)
(define fitness-channel-box
  (new check-box%
       [label ""] [value #t]
       [parent fitness-shelf]))
(define fitness-export #t)
(define fitness-export-box
  (new check-box%
       [label ""]
       [parent fitness-shelf]))

(define demographic-shelf
  (new horizontal-pane% [parent remote-list-pane]))
(new message% [parent demographic-shelf] [label "demographic"])
(define demographic-channel #f)
(define demographic-channel-box
  (new check-box%
       [label ""]
       [parent demographic-shelf]))
(define demographic-export #f)
(define demographic-export-box
  (new check-box%
       [label ""]
       [parent demographic-shelf]))

;; TEMP
(define one-run 10)
(define pause .03)
;; REMOTE
(define remote-pane
  (new horizontal-pane% [parent remote-list-pane]))

(define play-button
  (new button% [parent remote-pane] [label "play"]
       [callback (lambda (button event)
                   (trial-run))]))
(define pause-button
  (new button% [parent remote-pane] [label "pause"]))
(define stop-button
  (new button% [parent remote-pane] [label "stop"]
	[callback (lambda (b e)
	(begin
          (white-out!)
	(set! A (create-world row col))
	(set! series (list (vector 0 0)))))]))


;; SECOND COLUMN
(define control-panel (new vertical-pane% [parent big-frame]))

(new message% [parent control-panel]
     [label "configuration"])

; TEMP
(define cell-size 10)
(define (update-cell-size!)
  (set! cell-size (send cell-size-slider get-value)))

; cell-size: 5-15 pixels
(define cell-size-slider
  (new slider%
       [parent control-panel]
       [label "cell size"]
       [init-value 10]
       [min-value 5]
       [max-value 15]
       [callback (lambda (b e) (update-cell-size!))]
       ))
; pause-slider: 1-10 centisecond
(define pause (/ 5 100))
(define (update-pause!)
  (set! pause (/ (send pause-slider get-value)
                 100)))

(define pause-slider
  (new slider%
       [parent control-panel]
       [label "pause (centi'')"]
       [init-value 5]
       [min-value 1]
       [max-value 10]
       [callback (lambda (b e) (update-pause!))]))
; one-run: 1-10 * 10^2 rounds
(define one-run 100)
(define (update-one-run!)
  (set! one-run (* 100
                   (send one-run-slider get-value))))
(define one-run-slider
  (new slider%
       [parent control-panel]
       [label "one run (*10^2)"]
       [min-value 1]
       [max-value 10]
       [callback (lambda (b e) (update-one-run!))]))
; columns: 10-20
(define col 10)
(define (update-col!)
  (set! col (send column-slider get-value)))
(define column-slider
  (new slider%
       [parent control-panel]
       [label "column"]
       [min-value 10]
       [max-value 20]
       [callback (lambda (b e) (update-col!))]))
; rows: 10-20
(define row 10)
(define (update-row!)
  (set! row (send row-slider get-value)))
(define row-slider
  (new slider%
       [parent control-panel]
       [label "row"]
       [min-value 10]
       [max-value 20]
       [callback (lambda (b e) (update-row!))]))

(new message% [parent control-panel]
     [label "parameters (%)"])
; discount rate: %
(define discount-factor 0)
(define (update-discount-factor!)
  (set! discount-factor (/ (send discount-factor-slider get-value)
                           100)))
(define discount-factor-slider
  (new slider%
       [parent control-panel]
       [label "discount factor"]
       [min-value 0]
       [max-value 100]
       [callback (lambda (b e) (update-discount-factor!))]))

(define assortment 0)
(define (update-assortment!)
  (set! assortment (/ (send assortment-slider get-value)
                      100)))
(define assortment-slider
  (new slider%
       [parent control-panel]
       [label "assortment"]
       [min-value 0]
       [max-value 100]
       [callback (lambda (b e) (update-assortment!))]))
(define speed 5)
(define (update-speed!)
  (set! speed (send speed-slider get-value)))
(define speed-slider
  (new slider%
       [parent control-panel]
       [label "speed"]
       [init-value 5]
       [min-value 0]
       [max-value 100]
       [callback (lambda (b e) (update-speed!))]))

(define mutation-rate 0.5)
(define (update-mutation-rate!)
  (set! mutation-rate (/ (send mutation-rate-slider get-value)
                         100)))
(define mutation-rate-slider
  (new slider%
       [parent control-panel]
       [label "mutation rate"]
       [init-value 5]
       [min-value 0]
       [max-value 100]
       [callback (lambda (b e) (update-mutation-rate!))]))

(define repeated-times 1)
(define (update-repeated-times!)
  (set! repeated-times (send repeated-times-slider get-value)))

(define repeated-times-slider
  (new slider%
       [parent control-panel]
       [label "repeated times"]
       [init-value 1]
       [min-value 0]
       [max-value 100]
       [callback (lambda (b e) (update-repeated-times!))]))

(define complex-cost-rate 0)
(define (update-complex-cost-rate!)
  (set! complex-cost (/ (send complex-cost-rate-slider get-value)
                        100)))
(define complex-cost-rate-slider
  (new slider%
       [parent control-panel]
       [label "complex cost rate"]
       [min-value 0]
       [max-value 100]
       [callback (lambda (b e) (update-complex-cost-rate!))]))

;; THIRD COLUMN: TVs
(define channel-pane
  (new vertical-pane% [parent big-frame]))
(define population-and-payoff-space-shelf
  (new horizontal-pane% [parent channel-pane]))

; POPULATION (need to work on)
(define population-pane
  (new vertical-pane% [parent population-and-payoff-space-shelf]))
; TEMP (to be imported from slider))


(new message% [parent population-pane] [label "population"])
(define population-canvas
  (new canvas% [parent population-pane]))
(define population-dc (send population-canvas get-dc))
(define (draw-cell a-dc color x y)
  (send a-dc set-brush color 'solid)
  (send a-dc set-pen "white" 1 'solid)
  (send a-dc draw-rectangle x y cell-size cell-size))
(define (draw-population a-world)
  (for* ([i row]
         [j col])
        (let ([an-auto (vector-ref (vector-ref a-world i) j)])
          (draw-cell population-dc (mix-color an-auto) (* cell-size j)
                               (* cell-size i)))))



(define starting-strat-pane
  (new vertical-pane% [parent population-and-payoff-space-shelf]))
(new message% [parent starting-strat-pane] [label "starting strategy"])
(define starting-strat-canvas
  (new canvas% [parent starting-strat-pane]))
(define starting-strat-dc (send starting-strat-canvas get-dc))
(define (draw-starting-strats a-world)
  (for* ([i row]
	 [j col])
	(let* ([an-auto (vector-ref (vector-ref a-world i) j)]
	       [current-state (vector-first an-auto)])
	  (draw-cell starting-strat-dc
                     (sym->color
		      (vector-last
		       (vector-ref
			an-auto
			current-state)))
		     (* cell-size j)
		     (* cell-size i)))))

; PAYOFF SPACE
(define payoff-space-pane
  (new vertical-pane% [parent population-and-payoff-space-shelf]))
(new message% [parent payoff-space-pane] [label "payoff space"])
(define payoff-space-canvas (new canvas% [parent payoff-space-pane]))
(define payoff-space-dc (send payoff-space-canvas get-dc))

; TEMP (to be automatically calculated from configuration later)
(define min-pay -2)
(define max-pay 7)

(define (plot-payoff-space pay-book)
  (plot/dc (points (pack (vector-flatten pay-book))
                   #:x-min min-pay #:x-max max-pay
                   #:y-min min-pay #:y-max max-pay)
           payoff-space-dc
           0 0
           150 150)) ; TEMP should be equal to the mother pane

;; PAYOFF SERIES

;; draw payoff series
(define series (list (vector 0 0)))
(define (add-pair! ith-day)
  (set! series (append series (list (vector ith-day
                                            (population-mean A-pay)
                                            )))))

(new message% [parent channel-pane]
     [label "payoff time series"])
(define payoff-series-canvas
  (new canvas% [parent channel-pane]
       [min-width 500]))
(define payoff-series-dc
  (send payoff-series-canvas
        get-dc))
(define (plot-series ith-day)
  (begin
    (add-pair! ith-day)
    (plot/dc (lines series
                    #:x-min 0 #:x-max (+ 5 (length series))
                    #:y-min min-pay #:y-max max-pay)
             payoff-series-dc
             0 0
             500 150)))

;; FITNESS & DEMOGRAPHIC
(define fitness-and-demographic-pane
  (new horizontal-pane%
       [parent channel-pane]))
(define fitness-pane
  (new vertical-pane%
       [parent fitness-and-demographic-pane]))
(new message% [parent fitness-pane]
     [label "fitness"])
(define fitness-canvas
  (new canvas% [parent fitness-pane]
       [min-width 250]
       [min-height 150])) ; should be half width of series
(define fitness-dc
  (send fitness-canvas get-dc))

(define (draw-slice a-dc color start end y text)
  (send a-dc set-pen "white" 1 'transparent)
  (send a-dc set-brush color 'solid)
  (send a-dc draw-arc 10 10 120 120 start end)
  (send a-dc draw-rectangle 20 y 10 10)
  (send a-dc draw-text text 40 y))

(define (draw-pie a-world a-dc posn-in-report)
  (let* ([accum-list (posn-in-report (abridged-report!))]
	 [coor-list (append '(0) (convert->pi accum-list))]
	 [type-list (first (abridged-report!))]
	 [text-list (map
		     dna->string
		     type-list)])
    (for ([i (length accum-list)])
	 (draw-slice
          a-dc
	  (mix-color (list-ref type-list i))
	  (list-ref coor-list i)
	  (list-ref coor-list (+ i 1))
	  (+ 150 (* i 20))
	  (list-ref text-list i)))))

(define (draw-fitness-pie a-world)
  (draw-pie a-world fitness-dc third))

(define demographic-pane
  (new vertical-pane%
       [parent fitness-and-demographic-pane]))
(new message% [parent demographic-pane]
     [label "demographic"])
(define demographic-canvas
  (new canvas% [parent demographic-pane]
       [min-width 250])) ; half width of series
(define demographic-dc
  (send demographic-canvas get-dc))
(define (draw-demographic-pie a-world)
  (draw-pie a-world demographic-dc second))

(define (white-out!)
  (begin
    (send population-dc erase)
    (send starting-strat-dc erase)
    (send payoff-space-dc erase)
    (send payoff-series-dc erase)
    (send fitness-dc erase)
    (send demographic-dc erase)))

(send interface show #t)
