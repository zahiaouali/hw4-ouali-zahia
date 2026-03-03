#lang racket450
(require 2htdp/image)
(require 2htdp/universe)

(define CARD-SIZE 150)

(define MAX-POINTS 5)
(define MAX-COST 4)

(define RED "red")
(define GREEN "green")
(define BLUE "blue")


;; A Cost is a structure:
;;(cost Integer Integer Integer)
;; interpretation:
;; red, green, blue represent the number of tokens
;; required of each color to acquire a card.
;; Each value is between 0 and MAX-COST.
(struct cost (red green blue))

(define COST-EX
  (cost 1 2 3))

;; A Card is a structure:
;;(card Integer String Cost)
;; interpretation:
;; points is the card's point value (0 to MAX-POINTS)
;; color is one of "red", "green", "blue"
;; cost is the Cost required to acquire the card
(struct card (points color cost))

(define CARD-EX
  (card 3 RED (cost 1 0 2)))


;; A GameState is a structure:
;;   (gamestate (list Card Card Card Card Card Card Card Card Card))
;; interpretation:
;; cards represents the 9 cards on the board,
;; arranged left-to-right, top-to-bottom.
(struct gamestate (cards))
(define GAMESTATE-EX
  (gamestate
   (list CARD-EX CARD-EX CARD-EX
         CARD-EX CARD-EX CARD-EX
         CARD-EX CARD-EX CARD-EX)))

(define/contract (GameState-card g slot)
  (-> gamestate? integer? card?)
  (list-ref (gamestate-cards g) (- slot 1)))



(define/contract (Card-color c)
  (-> card? string?)
  (card-color c))


(define/contract (Card-point-value c)
  (-> card? integer?)
  (card-points c))

(define/contract (Card-red-cost c)
  (-> card? integer?)
  (cost-red (card-cost c)))

(define/contract (Card-green-cost c)
  (-> card? integer?)
  (cost-green (card-cost c)))


(define/contract (Card-blue-cost c)
  (-> card? integer?)
  (cost-blue (card-cost c)))


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
     (lambda (i)
       (random-card)))))

(define THIRD (/ CARD-SIZE 3))

(define/contract (draw-card c)
  (-> card? image?)

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

  ;; bottom tokens
  (define token-size (/ CARD-SIZE 3))
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

  ;; middle blank
  (define middle-section
    (rectangle CARD-SIZE THIRD "solid" "white"))

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

(define (run)
  (big-bang (random-gamestate)
    [to-draw draw-gamestate]
    [on-mouse handle-mouse]))