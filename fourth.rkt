#lang racket

(require 2htdp/image
         2htdp/universe
         racket/function
         racket/list
         racket/vector)

(define WIDTH 1024)
(define HEIGHT 600)
(define CANVAS (freeze (rectangle WIDTH HEIGHT "solid" "white")))
(define CELL 8)

(define WIDTH-RANGE (/ WIDTH CELL))
(define HEIGHT-RANGE (/ HEIGHT CELL))

;; Streams used in for loops to iterate over vectors
(define Y-AXIS (in-range HEIGHT-RANGE))
(define X-AXIS (in-range WIDTH-RANGE))

(define YMAX (sub1 HEIGHT-RANGE))
(define XMAX (sub1 WIDTH-RANGE))

;; Cell state is a pair? (real? . string?)
(define ALIVE (cons 1 "black"))
(define DEAD (cons 0 "white"))

;; (-> vector?)
;; Pre-load a vector of vectors with ALIVE/DEAD
(define (make-random-cells)
  (for/vector ([y Y-AXIS])
    (for/vector ([x X-AXIS])
      (if (zero? (modulo (random 5) 4))
        ALIVE DEAD))))

;; (-> real? real? vector? real?)
;; Return cell-state value for cell at x y but allow to wrap around edges
(define (get-neighbor x y cells)
  (define ny (cond
               [(< y 0) YMAX]
               [(> y YMAX) 0]
               [else y]))

  (define nx (cond
               [(< x 0) XMAX]
               [(> x XMAX) 0]
               [else x]))

  (car (vector-ref (vector-ref cells ny) nx)))

;; (-> real? real? vector? real?)
;; Find the neighbors of the active cell at xy and return the sum
;; Used to determine how many neighbors are alive
(define (get-neighbors x y cells)
  (define ns (list
              ;; top
              (get-neighbor (- x 1) (- y 1) cells)
              (get-neighbor x (- y 1) cells)
              (get-neighbor (+ x 1) (- y 1) cells)
              ;; sides
              (get-neighbor (- x 1) y cells)
              (get-neighbor (+ x 1) y cells)
              ;; bottom
              (get-neighbor (- x 1) (+ y 1) cells)
              (get-neighbor x (+ y 1) cells)
              (get-neighbor (+ x 1) (+ y 1) cells)))

  (apply + ns))

;; (-> vector? vector? real? vector?)
;; Loop over the row and for each cell get the sum of its neighbors and
;; decide if the cell lives (1) or dies (0) based on that sum.
(define (apply-rules cells row y)
  (for/vector ([x X-AXIS])
    (define cell (vector-ref row x))
    (define cv (car cell))
    (define neighbors (get-neighbors x y cells))
    (cond
      [(and (= cv 1) (< neighbors 2)) DEAD]
      [(and (= cv 1) (or (= neighbors 2) (eq? neighbors 3))) ALIVE]
      [(and (= cv 1) (> neighbors 3)) DEAD]
      [(and (zero? cv) (= neighbors 3)) ALIVE]
      [else cell])))

;; (-> vector? bitmap?)
(define (make-row-bitmap row)
  (define ls (vector->list (vector-map cdr row)))
  (scale CELL (color-list->bitmap ls WIDTH-RANGE 1)))

;; (-> vector? vector?)
;; On each tick zip over the vectors and apply rules, switching cell
;; state depending on neighbor conditions. Returns a new vector.
(define (tick cells)
  (for/vector ([y Y-AXIS])
    (apply-rules cells (vector-ref cells y) y)))

;; (-> vector? bitmap?)
;; Take the cells vector and convert to a bitmap for display
(define (draw cells)
  (for/fold ([canvas CANVAS])
            ([y-index Y-AXIS])
    (place-image
     (make-row-bitmap (vector-ref cells y-index))
     (/ WIDTH 2)
     (+ (* y-index CELL) (/ CELL 2))
     canvas)))

(define (main)
  (define t (big-bang (make-random-cells)
                      ;(display-mode 'fullscreen)
                      (on-tick tick)
                      (to-draw draw)))
  0)

(main)
