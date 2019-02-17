#lang racket

(require 2htdp/universe
         data/ring-buffer
         openssl/sha1
         racket/draw
         racket/function
         racket/list
         racket/vector)

(define WIDTH 1020)
(define HEIGHT 600)
(define CELL 5) ;; giant blocks for raspi

(define WIDTH-RANGE (/ WIDTH CELL))
(define HEIGHT-RANGE (/ HEIGHT CELL))

;; Streams used in for loops to iterate over vectors
(define Y-AXIS (in-range HEIGHT-RANGE))
(define X-AXIS (in-range WIDTH-RANGE))

(define YMAX (sub1 HEIGHT-RANGE))
(define XMAX (sub1 WIDTH-RANGE))

(struct alive (x y) #:transparent)
(struct dead (x y) #:transparent)

(define OUTP (open-output-string))

(define rb (empty-ring-buffer 3))

;; [Effect]
;; (-> void)
;; Reset the ring-buffer to "0" "1" "2"
(define (reset-buffer!)
  (for ([i (in-range 3)])
    (ring-buffer-push! rb (number->string i))))

;; (-> vector?)
;; Pre-load a vector of vectors with ALIVE/DEAD
(define (make-random-cells)
  (build-vector
   HEIGHT-RANGE
   (λ (y)
     (build-vector WIDTH-RANGE
                   (λ (x) (if (zero? (random 3))
                              (dead (* CELL x) (* CELL y))
                              (alive (* CELL x) (* CELL y))))))))


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

  (if (alive? (vector-ref (vector-ref cells ny) nx))
      1
      0))

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
(define (apply-rules old-cells old-row y)
  (define new-row (vector-copy old-row))
  (for ([cell (in-vector old-row)]
        [x X-AXIS])
    (write (if (alive? cell) 1 0) OUTP)
    (define neighbors (get-neighbors x y old-cells))
    (define new-x (* CELL x))
    (define new-y (* CELL y))
    (cond
      [(and (dead? cell) (= neighbors 3))
       (vector-set! new-row x (alive new-x new-y))]
      [(and (alive? cell) (< neighbors 2))
       (vector-set! new-row x (dead new-x new-y))]
      [(and (alive? cell) (or (= neighbors 2) (eq? neighbors 3)))
       (vector-set! new-row x (alive new-x new-y))]
      [(and (alive? cell) (> neighbors 3))
       (vector-set! new-row x (dead new-x new-y))]
      [else (vector-set! new-row x cell)]))
  new-row)

(define (push-cell-state!)
  (ring-buffer-push!
   rb (sha1
       (open-input-string
        (bytes->string/utf-8
         (get-output-bytes OUTP #t) #\?)))))

;; (-> vector? vector?)
;; On each tick zip over the vectors and apply rules, switching cell
;; state depending on neighbor conditions. Returns a new vector.
(define (tick cells)
  ;; If we're in a stable oscillation then reset the buffer and scramble
  ;; the cells to "start over"
  (define old-cells (if (check-buffer cells)
                        (begin
                          (reset-buffer!)
                          (make-random-cells))
                        (vector-copy cells)))

  (define new-cells (vector-copy old-cells))

  (for ([row (in-vector old-cells)]
        [y Y-AXIS])
    (vector-set! new-cells y (apply-rules old-cells row y)))

  new-cells)

;; [Effect]
;; (-> vector? bitmap?)
;; Take the cells vector and convert to a bitmap for display - also computes the
;; SHA1 of the cell state and pushes into a ring-buffer
(define (draw cells)
  (push-cell-state!)

  (define target (make-bitmap WIDTH HEIGHT))
  (define dc (new bitmap-dc% [bitmap target]))
  (send dc set-brush "black" 'solid)

  (for* ([y-index (in-range HEIGHT-RANGE)]
         [x-index (in-range WIDTH-RANGE)])
    (define cell (vector-ref (vector-ref cells y-index) x-index))
    (when (alive? cell)
      (send dc draw-rectangle
            (alive-x cell) (alive-y cell)
            CELL CELL)))

  target)

(define (check-buffer cells)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (equal? (ring-buffer-ref rb 0) (ring-buffer-ref rb 2))))

(define (main)
  (define t (big-bang (make-random-cells)
                      ;(display-mode 'fullscreen)
                      ;(stop-when check-buffer)
                      (on-tick tick)
                      (to-draw draw)))
  void)

(main)
