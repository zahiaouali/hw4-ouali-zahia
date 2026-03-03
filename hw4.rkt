#lang racket450
(require 2htdp/image)
(require 2htdp/universe)

;; Constants
(define CARD-SIZE 150)
(define MAX-POINTS 5)
(define MAX-COST 4)

(define RED "red")
(define GREEN "green")
(define BLUE "blue")

;; Data Definitions
(struct cost (red green blue))
(struct card (points color cost))
(struct gamestate (cards))

;; Required Constructors (AUTOGRADER)
(define (mk-Cost r g b)
  (cost r g b))

(define (mk-Card p c co)
  (card p c co))

(define (mk-GameState cards)
  (gamestate cards))

;; Required Function
(define (GameState-card g slot)
  (list-ref (gamestate-cards g) (- slot 1)))

;; Random Generators

(define (random-card)
  (mk-Card
   (random (+ MAX-POINTS 1))
   (list-ref (list RED GREEN BLUE) (random 3))
   (mk-Cost
    (random (+ MAX-COST 1))
    (random (+ MAX-COST 1))
    (random (+ MAX-COST 1)))))

(define (random-gamestate)
  (mk-GameState
   (build-list 9 (lambda (i) (random-card)))))

;; Drawing
(define THIRD (/ CARD-SIZE 3))

(define (draw-card c)

  (define top-section
    (overlay/align
     "left" "center"
     (overlay
      (text (number->string (card-points c)) 20 "black")
      (square THIRD "solid" "white"))
     (rectangle CARD-SIZE THIRD "solid" (card-color c))))

  (define middle-section
    (rectangle CARD-SIZE THIRD "solid" "white"))

  (define bottom-section
    (beside
     (overlay
      (text (number->string (cost-red (card-cost c))) 15 "black")
      (circle (/ THIRD 2) "solid" RED))
     (overlay
      (text (number->string (cost-green (card-cost c))) 15 "black")
      (circle (/ THIRD 2) "solid" GREEN))
     (overlay
      (text (number->string (cost-blue (card-cost c))) 15 "black")
      (circle (/ THIRD 2) "solid" BLUE))))

  (above top-section middle-section bottom-section))

(define (draw-gamestate g)
  (define cards (gamestate-cards g))

  (above
   (beside
    (draw-card (list-ref cards 0))
    (draw-card (list-ref cards 1))
    (draw-card (list-ref cards 2)))

   (beside
    (draw-card (list-ref cards 3))
    (draw-card (list-ref cards 4))
    (draw-card (list-ref cards 5)))

   (beside
    (draw-card (list-ref cards 6))
    (draw-card (list-ref cards 7))
    (draw-card (list-ref cards 8)))))

;; Mouse Handler

(define (handle-mouse g x y me)
  (if (string=? me "button-down")
      (let* ([col (quotient x CARD-SIZE)]
             [row (quotient y CARD-SIZE)]
             [index (+ (* row 3) col)]
             [new-cards
              (list-set (gamestate-cards g)
                        index
                        (random-card))])
        (mk-GameState new-cards))
      g))

;; Run
(define (run)
  (big-bang (random-gamestate)
    [to-draw draw-gamestate]
    [on-mouse handle-mouse]))