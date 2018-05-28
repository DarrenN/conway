#lang racket

(require 2htdp/image
         2htdp/universe)

(define CELL 8)
(define WIDTH 1024)
(define HEIGHT 600)
(define CANVAS (freeze (rectangle WIDTH HEIGHT "solid" "white")))
(define WHITE (freeze (rectangle CELL CELL "solid" "white")))
(define BLACK (freeze (rectangle CELL CELL "solid" "black")))

(define (make-cs)
  (for/list ([i (in-range (/ WIDTH CELL))])
    (if (zero? (modulo (random 5) 4))
        "black" "white")))

(define (make-row ls)
  (apply beside (map (λ (x) (if (equal? x "white") WHITE BLACK)) ls)))

;; HUGE SPEEDUP
(define (make-row2 ls)
  (scale CELL (color-list->bitmap ls (length ls) 1)))

(define (draw x)
  (for/fold ([canvas CANVAS])
            ([i (in-range (/ HEIGHT CELL))])
   (place-image
    (make-row (make-cs)) (/ WIDTH 2) (+ (* i CELL) (/ CELL 2)) canvas)))

(define (draw2 x)
  (for/fold ([canvas CANVAS])
            ([i (in-range (/ HEIGHT CELL))])
   (place-image
    (make-row2 (make-cs)) (/ WIDTH 2) (+ (* i CELL) (/ CELL 2)) canvas)))


(define (main)
  (animate draw2))

;;(main)

;(time (draw2 0))
;(time (draw 0))


#|
(for ([i (in-range 20)])
  (time (draw 0)))
|#
;; could use to operate on a vector and build up a list of
;; only "live" cells
#|
(for/fold ([ac '()]
           [ps '()])
          ([i (make-cs)])
  (values
   (cons (~a i "-cell") ac)
   (if (equal? i "black")
       (cons i ps)
       ps)))
|#
