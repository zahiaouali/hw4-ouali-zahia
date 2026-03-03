#lang racket450/testing
(require "hw4.rkt")
(require 2htdp/image)

;; Data Accessor Tests

(define TEST-COST (cost 1 2 3))
(define TEST-CARD (card 4 RED TEST-COST))

(check-equal? (Card-color TEST-CARD) RED)
(check-equal? (Card-point-value TEST-CARD) 4)

(check-equal? (Card-red-cost TEST-CARD) 1)
(check-equal? (Card-green-cost TEST-CARD) 2)
(check-equal? (Card-blue-cost TEST-CARD) 3)


;; GameState-card Tests

(define GS-TEST
  (gamestate
   (list TEST-CARD TEST-CARD TEST-CARD
         TEST-CARD TEST-CARD TEST-CARD
         TEST-CARD TEST-CARD TEST-CARD)))

(check-equal? (GameState-card GS-TEST 1) TEST-CARD)
(check-equal? (GameState-card GS-TEST 9) TEST-CARD)


;; random-card Tests

(define RC (random-card))

(check-true (card? RC))
(check-true (<= 0 (Card-point-value RC) MAX-POINTS))

(check-true (<= 0 (Card-red-cost RC) MAX-COST))
(check-true (<= 0 (Card-green-cost RC) MAX-COST))
(check-true (<= 0 (Card-blue-cost RC) MAX-COST))

(check-true (or (string=? (Card-color RC) RED)
                (string=? (Card-color RC) GREEN)
                (string=? (Card-color RC) BLUE)))


;; random-gamestate Tests

(define RG (random-gamestate))

(check-true (gamestate? RG))
(check-equal? (length (gamestate-cards RG)) 9)


;; Drawing Tests

(check-true (image? (draw-card TEST-CARD)))
(check-true (image? (draw-gamestate GS-TEST)))



;; handle-mouse Tests

(define BEFORE GS-TEST)

;; Non button-down event should not change state
(check-equal?
 (handle-mouse BEFORE 10 10 "move")
 BEFORE)

;; Button-down should return a gamestate
(check-true
 (gamestate?
  (handle-mouse BEFORE 10 10 "button-down")))

