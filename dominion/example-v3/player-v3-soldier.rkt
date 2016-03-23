#! /home/mflatt/proj/plt/racket/bin/racket
#lang racket
(require "test.rkt"
         "cards.rkt"
         "player.rkt")

;; ----------------------------------------

(define (try-action state)
  (define hand (get-hand state))
  (and (positive? (get-num 'actions state))
       (cond 
         [(member 'militia hand)
          `(act militia)]
         [(member 'moat hand)
          `(act moat)]
         [else #f])))

(test (try-action '((actions 1) (hand) (supply))) #f)
(test (try-action '((actions 1) (hand copper moat))) '(act moat))
(test (try-action '((actions 1) (hand militia moat))) '(act militia))

;; ----------------------------------------

(define (try-buy state)
  (define coins (get-num 'coins state))
  (define supply (get-supply state))
  (and (positive? (get-num 'buys state))
       (for/or ([card '(province duchy militia moat)])
         (and (coins . >= . (cost-of card))
              (member card supply)
              `(buy ,card)))))

(test (try-buy '((buys 0) (supply militia moat) (coins 8))) #f)
(test (try-buy '((buys 1) (supply militia moat) (coins 8))) '(buy militia))
(test (try-buy '((buys 1) (supply militia moat) (coins 3))) '(buy moat))

;; ----------------------------------------

(run-player try-action try-buy)
