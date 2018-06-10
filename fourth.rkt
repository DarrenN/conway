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
(define CANVAS (freeze (rectangle WIDTH HEIGHT "solid" "white")))
(define CELL 15) ;; giant blocks for raspi

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
  (for/vector ([y Y-AXIS])
    (for/vector ([x X-AXIS])
      (if (zero? (modulo (random 11) 10))
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
    (write (number->string cv) OUTP)
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

  (for/vector ([y Y-AXIS])
    (apply-rules cs (vector-ref cs y) y)))

;; [Effect]
;; (-> vector? bitmap?)
;; Take the cells vector and convert to a bitmap for display - also computes the
;; SHA1 of the cell state and pushes into a ring-buffer
(define (draw cells)
  (push-cell-state!)

  (define new-image
    (for/fold ([canvas CANVAS])
              ([y-index Y-AXIS])
      (place-image
       (make-row-bitmap (vector-ref cells y-index))
       (/ WIDTH 2)
       (+ (* y-index CELL) (/ CELL 2))
       canvas)))

  (cond
    [SHOW-LABELS (place-image (create-state-labels) (/ WIDTH 2) 30 new-image)]
    [else new-image]))

(define (check-buffer cells)
  (equal? (ring-buffer-ref rb 0) (ring-buffer-ref rb 2)))

(define (main)
  (define t (big-bang (make-random-cells)
                      (display-mode 'fullscreen)
                      ;(stop-when check-buffer)
                      (on-tick tick)
                      (to-draw draw)))
  void)

(main)
