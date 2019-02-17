#lang racket

(require 2htdp/image
         2htdp/universe
         data/ring-buffer
         openssl/sha1
         racket/function
         racket/list
         racket/vector)

(define WIDTH 1020)
(define HEIGHT 600)
(define CELL 10) ;; giant blocks for raspi
(define CELL-CENTER (/ CELL 2))

(define CANVAS (freeze (rectangle WIDTH HEIGHT "solid" "white")))
(define SQUARE (freeze (square CELL "solid" "black")))

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

(define SHOW-LABELS #f)

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
  (for/vector #:length HEIGHT-RANGE ([y Y-AXIS])
    (for/vector #:length WIDTH-RANGE ([x X-AXIS])
      (if (zero? (modulo (random 11) 10))
          (alive (* CELL x) (* CELL y))
          (dead (* CELL x) (* CELL y))))))

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
(define (apply-rules cells row y)
  (for/vector #:length WIDTH ([x X-AXIS])
    (define cell (vector-ref row x))
    (write (if (alive? cell) 1 0) OUTP)
    (define neighbors (get-neighbors x y cells))
    (define new-x (* CELL x))
    (define new-y (* CELL y))
    (cond
      [(and (dead? cell) (= neighbors 3)) (alive new-x new-y)]
      [(and (alive? cell) (< neighbors 2)) (dead new-x new-y)]
      [(and (alive? cell) (or (= neighbors 2) (eq? neighbors 3)))
       (alive new-x new-y)]
      [(and (alive? cell) (> neighbors 3)) (dead new-x new-y)]
      [else cell])))

(define (push-cell-state!)
  (ring-buffer-push!
   rb (sha1
       (open-input-string
        (bytes->string/utf-8
         (get-output-bytes OUTP #t) #\?)))))

(define (create-state-labels)
  (above
   (text (ring-buffer-ref rb 0) 16 "red")
   (text (ring-buffer-ref rb 1) 16 "blue")
   (text (ring-buffer-ref rb 2) 16 "red")))

;; (-> vector? vector?)
;; On each tick zip over the vectors and apply rules, switching cell
;; state depending on neighbor conditions. Returns a new vector.
(define (tick cells)
  ;; If we're in a stable oscillation then reset the buffer and scramble
  ;; the cells to "start over"
  (define cs (if (check-buffer cells)
                 (begin
                   (reset-buffer!)
                   (make-random-cells))
                 cells))

  (for/vector #:length HEIGHT ([row (in-vector cells)]
                               [y Y-AXIS])
    (apply-rules cs row y)))

;; [Effect]
;; (-> vector? bitmap?)
;; Take the cells vector and convert to a bitmap for display - also computes the
;; SHA1 of the cell state and pushes into a ring-buffer
(define (draw cells)
  (push-cell-state!)

  (define new-image
    (for*/fold ([canvas CANVAS])
               ([y-index (in-vector HEIGHT-RANGE)]
                [x-index (in-range WIDTH-RANGE)])
      (define point (vector-ref (vector-ref cells y-index) x-index))
      (if (alive? point)
          (place-image SQUARE
                       (+ (alive-x point) CELL-CENTER)
                       (+ (alive-y point) CELL-CENTER)
                       canvas)
          canvas)))

  (cond
    [SHOW-LABELS (place-image (create-state-labels) (/ WIDTH 2) 30 new-image)]
    [else new-image]))

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
