#lang racket/base

(require 2htdp/image
         2htdp/universe
         racket/function
         racket/list
         racket/vector)

(struct world (points canvas) #:transparent)

(define HEIGHT 320)
(define WIDTH (* HEIGHT 2))

(define PIXEL-SIZE 10)

(define COLUMNS (make-vector (/ WIDTH PIXEL-SIZE) 0))
(define FIELD (make-vector (/ HEIGHT PIXEL-SIZE) COLUMNS))

(define ROWIDX (range (/ HEIGHT PIXEL-SIZE)))
(define COLIDX (range (/ WIDTH PIXEL-SIZE)))

(define ROWMAX (sub1 (length ROWIDX)))
(define COLMAX (sub1 (length COLIDX)))

(define ON (square PIXEL-SIZE "solid" "black"))
(define OFF (square PIXEL-SIZE "solid" "white"))

(define (randomize-row row)
  (vector-map (λ (v) (random 2)) row))

(define (randomize-fields field)
  (vector-map randomize-row field))

(define (get-neighbor x y ps)
  (cond
    [(or (< y 0) (> y ROWMAX)) 0]
    [(or (< x 0) (> x COLMAX)) 0]
    [else (vector-ref (vector-ref ps y) x)]))

(define (get-neighbors x y ps)
  (define top (list (get-neighbor (- x 1) (- y 1) ps)
                    (get-neighbor x (- y 1) ps)
                    (get-neighbor (+ x 1) (- y 1) ps)))
  (define sides (list (get-neighbor (- x 1) y ps)
                      (get-neighbor (+ x 1) y ps)))
  (define bottom (list (get-neighbor (- x 1) (+ y 1) ps)
                       (get-neighbor x (+ y 1) ps)
                       (get-neighbor (+ x 1) (+ y 1) ps)))
  (apply + (flatten (list top sides bottom))))

(define (apply-rules ps row y)
  (for/vector ([x COLIDX])
    (define p (vector-ref row x))
    (define neighbors (get-neighbors x y ps))
    (cond
      [(and (eq? p 1) (< neighbors 2)) 0]
      [(and (eq? p 1) (or (eq? neighbors 2) (eq? neighbors 3))) 1]
      [(and (eq? p 1) (> neighbors 3)) 0]
      [(and (zero? p) (eq? neighbors 3)) 1]
      [else p])))

(define (tick w)
  (define c (world-canvas w))
  (define ps (world-points w))
  (define new-ps (for/vector ([y ROWIDX])
                   (apply-rules ps (vector-ref ps y) y)))
  (struct-copy world w [points new-ps]))

(define (draw w)
  (define c (world-canvas w))
  (define ps (world-points w))
  (foldl (λ (idx canvas)
           (define row (vector-ref ps idx))
           (define row-canvas (rectangle WIDTH PIXEL-SIZE "solid" "white"))
           (define row-image
             (foldl (λ (cidx canvas)
                      (define pv (vector-ref row cidx))
                      (define shape (if (eq? pv 1) ON OFF))
                      (underlay/xy canvas (* cidx PIXEL-SIZE) 0 shape))
                    row-canvas COLIDX))
           (underlay/xy canvas 0 (* idx PIXEL-SIZE) row-image)) c ROWIDX))

(big-bang (world (randomize-fields FIELD)
                 (rectangle WIDTH HEIGHT "solid" "white"))
          (on-tick tick)
          (to-draw draw))
