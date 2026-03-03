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
(struct Cost (red green blue))
(struct Card (points color cost))
(struct GameState (cards))

;; Required Constructors (AUTOGRADER EXPECTS THESE)

(define (mk-Cost r g b)
  (Cost r g b))

(define (mk-Card p c co)
  (Card p c co))

(define (mk-GameState cards)
  (GameState cards))

;; Required API Wrappers

(define (Card-point-value c)
  (Card-points c))

(define (Card-red-cost c)
  (Cost-red (Card-cost c)))

(define (Card-green-cost c)
  (Cost-green (Card-cost c)))

(define (Card-blue-cost c)
  (Cost-blue (Card-cost c)))

(define (GameState-card g slot)
  (list-ref (GameState-cards g) (- slot 1)))

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
      (text (number->string (Card-points c)) 20 "black")
      (square THIRD "solid" "white"))
     (rectangle CARD-SIZE THIRD "solid" (Card-color c))))

  (define middle-section
    (rectangle CARD-SIZE THIRD "solid" "white"))

  (define bottom-section
    (beside
     (overlay
      (text (number->string (Cost-red (Card-cost c))) 15 "black")
      (circle (/ THIRD 2) "solid" RED))
     (overlay
      (text (number->string (Cost-green (Card-cost c))) 15 "black")
      (circle (/ THIRD 2) "solid" GREEN))
     (overlay
      (text (number->string (Cost-blue (Card-cost c))) 15 "black")
      (circle (/ THIRD 2) "solid" BLUE))))

  (above top-section middle-section bottom-section))

(define (draw-gamestate g)
  (define cards (GameState-cards g))

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

;; Mouse
(define (handle-mouse g x y me)
  (if (string=? me "button-down")
      (let* ([col (quotient x CARD-SIZE)]
             [row (quotient y CARD-SIZE)]
             [index (+ (* row 3) col)]
             [cards (GameState-cards g)])
        (if (and (>= col 0) (< col 3)
                 (>= row 0) (< row 3))
            (mk-GameState
             (list-set cards index (random-card)))
            g))
      g))

(define (run)
  (big-bang (random-gamestate)
    [to-draw draw-gamestate]
    [on-mouse handle-mouse]))