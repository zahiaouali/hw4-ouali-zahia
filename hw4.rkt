#lang racket450
(require 2htdp/image)
(require 2htdp/universe)

(define CARD-SIZE 150)

(define MAX-POINTS 5)
(define MAX-COST 4)

(define RED "red")
(define GREEN "green")
(define BLUE "blue")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct cost (red green blue))
(struct card (points color cost))
(struct gamestate (cards))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Required API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (GameState-card g slot)
  (-> gamestate? integer? card?)
  (list-ref (gamestate-cards g) (- slot 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random Generators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (random-card)
  (-> card?)
  (define pts (random (+ MAX-POINTS 1)))

  (define color-index (random 3))
  (define col
    (cond
      [(= color-index 0) RED]
      [(= color-index 1) GREEN]
      [else BLUE]))

  (define r (random (+ MAX-COST 1)))
  (define g (random (+ MAX-COST 1)))
  (define b (random (+ MAX-COST 1)))

  (card pts col (cost r g b)))

(define/contract (random-gamestate)
  (-> gamestate?)
  (gamestate
   (build-list 9
     (lambda (i) (random-card)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define THIRD (/ CARD-SIZE 3))

(define/contract (draw-card c)
  (-> card? image?)

  ;; Top
  (define top-rect
    (rectangle CARD-SIZE THIRD "solid" (card-color c)))

  (define point-box
    (overlay
     (text (number->string (card-points c)) 20 "black")
     (square THIRD "solid" "white")))

  (define top-section
    (overlay/align
     "left" "center"
     point-box
     top-rect))

  ;; Middle
  (define middle-section
    (rectangle CARD-SIZE THIRD "solid" "white"))

  ;; Bottom
  (define token-size THIRD)
  (define radius (/ token-size 2))

  (define red-token
    (overlay
     (text (number->string (cost-red (card-cost c))) 15 "black")
     (circle radius "solid" RED)))

  (define green-token
    (overlay
     (text (number->string (cost-green (card-cost c))) 15 "black")
     (circle radius "solid" GREEN)))

  (define blue-token
    (overlay
     (text (number->string (cost-blue (card-cost c))) 15 "black")
     (circle radius "solid" BLUE)))

  (define bottom-section
    (beside red-token green-token blue-token))

  (above top-section middle-section bottom-section))

(define/contract (draw-gamestate g)
  (-> gamestate? image?)

  (define cards (gamestate-cards g))

  (define row1
    (beside
     (draw-card (list-ref cards 0))
     (draw-card (list-ref cards 1))
     (draw-card (list-ref cards 2))))

  (define row2
    (beside
     (draw-card (list-ref cards 3))
     (draw-card (list-ref cards 4))
     (draw-card (list-ref cards 5))))

  (define row3
    (beside
     (draw-card (list-ref cards 6))
     (draw-card (list-ref cards 7))
     (draw-card (list-ref cards 8))))

  (above row1 row2 row3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse Handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (handle-mouse g x y me)
  (-> gamestate? integer? integer? mouse-event? gamestate?)

  (if (string=? me "button-down")
      (let* ([col (quotient x CARD-SIZE)]
             [row (quotient y CARD-SIZE)]
             [index (+ (* row 3) col)]
             [new-card (random-card)]
             [new-cards (list-set (gamestate-cards g) index new-card)])
        (gamestate new-cards))
      g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run (DO NOT AUTO-CALL)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run)
  (big-bang (random-gamestate)
    [to-draw draw-gamestate]
    [on-mouse handle-mouse]))