#! /home/mflatt/proj/plt/racket/bin/racket
#lang racket
(require "test.rkt"
         "cards.rkt"
         "player.rkt")

;; ----------------------------------------

(define (try-action state)
  (define hand (get-hand state))
  (define supply (get-supply state))  
  (and (positive? (get-num 'actions state))
       (cond 
         [(and (member 'mine hand)
               (member 'silver hand)
               (member 'gold supply))
          `(act mine silver gold)]
         [(and (member 'mine hand)
               (member 'copper hand)
               (member 'silver supply))
          `(act mine copper silver)]
         [else #f])))

(test (try-action '((actions 0) (hand) (supply))) #f)
(test (try-action '((actions 0) (hand mine copper) (supply silver))) #f)
(test (try-action '((actions 1) (hand mine copper) (supply silver))) '(act mine copper silver))
(test (try-action '((actions 1) (hand mine silver) (supply gold))) '(act mine silver gold))
(test (try-action '((actions 1) (hand mine copper silver) (supply silver gold))) '(act mine silver gold))

;; ----------------------------------------

(define (try-buy state)
  (define coins (get-num 'coins state))
  (define supply (get-supply state))
  (and (positive? (get-num 'buys state))
       (for/or ([card '(province mine gold silver duchy copper)])
         (and (coins . >= . (cost-of card))
              (member card supply)
              `(buy ,card)))))

(test (try-buy '((buys 0) (supply province silver) (coins 8))) #f)
(test (try-buy '((buys 1) (supply province silver) (coins 8))) '(buy province))
(test (try-buy '((buys 1) (supply province silver) (coins 7))) '(buy silver))
(test (try-buy '((buys 2) (supply province copper) (coins 0))) '(buy copper))

;; ----------------------------------------

(run-player try-action try-buy)
