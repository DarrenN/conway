#lang racket/base

(require 2htdp/image
         2htdp/universe
         racket/function
         racket/list
         racket/vector)

(struct cell (state x y pixel) #:transparent)
(struct world (cells canvas) #:transparent)

(define WIDTH 1024)
(define HEIGHT 600)

(define PIXEL-SIZE 8)
(define PIXEL (square PIXEL-SIZE "solid" "black"))
(define CANVAS (rectangle WIDTH HEIGHT "solid" "white"))

(define Y-AXIS (range (/ HEIGHT PIXEL-SIZE))) ;; Y-axis
(define X-AXIS (range (/ WIDTH PIXEL-SIZE))) ;; X-axis

;; Create a randomized starting field
(define FIELD (for/vector ([y Y-AXIS])
                (for/vector ([x X-AXIS])
                  (cell (random 2) (* x PIXEL-SIZE) (* y PIXEL-SIZE) PIXEL))))

(define YMAX (sub1 (length Y-AXIS)))
(define XMAX (sub1 (length X-AXIS)))

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

  (cell-state (vector-ref (vector-ref cells ny) nx)))

(define (get-neighbors x y cells)
  (define top (list (get-neighbor (- x 1) (- y 1) cells)
                    (get-neighbor x (- y 1) cells)
                    (get-neighbor (+ x 1) (- y 1) cells)))
  (define sides (list (get-neighbor (- x 1) y cells)
                      (get-neighbor (+ x 1) y cells)))
  (define bottom (list (get-neighbor (- x 1) (+ y 1) cells)
                       (get-neighbor x (+ y 1) cells)
                       (get-neighbor (+ x 1) (+ y 1) cells)))
  (apply + (flatten (list top sides bottom))))

(define (apply-rules cells row y)
  (for/vector ([x X-AXIS])
    (define cl (vector-ref row x))
    (define alive (cell-state cl))
    (define neighbors (get-neighbors x y cells))
    (cond
      [(and (= alive 1) (< neighbors 2)) (struct-copy cell cl [state 0])]
      [(and (= alive 1) (or (= neighbors 2) (eq? neighbors 3)))
       (struct-copy cell cl [state 1])]
      [(and (= alive 1) (> neighbors 3)) (struct-copy cell cl [state 0])]
      [(and (zero? alive) (= neighbors 3)) (struct-copy cell cl [state 1])]
      [else cl])))

(define (tick w)
  (define c (world-canvas w))
  (define cells (world-cells w))
  (define new-cells (for/vector ([y Y-AXIS])
                      (apply-rules cells (vector-ref cells y) y)))
  (struct-copy world w [cells new-cells]))

(define row-canvas (rectangle WIDTH PIXEL-SIZE "solid" "white"))

;; Big win by just freezing once before returning to big-bang
;; we're only doing one underlay call and using the x/y of each cell to do
;; placement
(define (draw w)
  (define c (world-canvas w))
  (define cells (world-cells w))
  (freeze
   (foldl (λ (yidx canvas)
            (define row (vector-ref cells yidx))
            (foldl (λ (xidx rcanvas)
                     (define cl (vector-ref row xidx))
                     (if (= (cell-state cl) 1)
                         (underlay/xy rcanvas (cell-x cl) (cell-y cl)
                                      (cell-pixel cl))
                         rcanvas))
                   canvas X-AXIS))
          CANVAS Y-AXIS)))

(define (main)
  (define t (big-bang (world FIELD (freeze CANVAS))
                      (on-tick tick)
                      (to-draw draw)))
  0)

(main)
