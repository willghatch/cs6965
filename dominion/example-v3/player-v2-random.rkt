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
         [(member 'market hand)
          `(act market)]
         [(member 'village hand)
          `(act village)]
         [(and (member 'cellar hand)
               (for/or ([c '(estate duchy province)])
                 (memq c hand)))
          `(act cellar ,@(for/list ([c hand]
                                    #:when (memq c '(estate duchy province)))
                           c))]
         [(member 'woodcutter hand)
          `(act woodcutter)]
         [(member 'smithy hand)
          `(act smithy)]
         [(and (member 'workshop hand)
               (for/or ([c (append (shuffle '(smithy village woodcutter))
                                   '(silver))])
                 (and (member c supply) c)))
          => (lambda (c)
               `(act workshop ,c))]
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
(test (try-action '((actions 1) (hand market copper silver) (supply silver gold))) '(act market))
(test (try-action '((actions 1) (hand village copper silver) (supply silver gold))) '(act village))
(test (try-action '((actions 1) (hand woodcutter copper silver) (supply silver gold))) '(act woodcutter))
(test (try-action '((actions 1) (hand smithy copper silver) (supply silver gold))) '(act smithy))
(test (try-action '((actions 1) (hand workshop copper silver) (supply silver gold))) '(act workshop silver))
(test (try-action '((actions 1) (hand workshop copper silver) (supply smithy silver gold))) '(act workshop smithy))
(test (try-action '((actions 1) (hand cellar copper silver) (supply smithy silver gold))) #f)
(test (try-action '((actions 1) (hand cellar copper estate province) (supply))) '(act cellar estate province))

;; ----------------------------------------

(define BUY-COPPER? #f)

(define (try-buy state)
  (define coins (get-num 'coins state))
  (define supply (get-supply state))
  (and (positive? (get-num 'buys state))
       (for/or ([card (append
                       '(province gold)
                       '(mine market village smithy workshop woodcutter cellar)
                       '(silver duchy)
                       (if BUY-COPPER? '(copper) '()))])
         (and (coins . >= . (cost-of card))
              (member card supply)
              `(buy ,card)))))

(test (try-buy '((buys 0) (supply province silver) (coins 8))) #f)
(test (try-buy '((buys 1) (supply province silver) (coins 8))) '(buy province))
(test (try-buy '((buys 1) (supply province silver) (coins 7))) '(buy silver))
(test (try-buy '((buys 2) (supply province copper) (coins 0))) (if BUY-COPPER?
                                                                   '(buy copper)
                                                                   #f))
(test (try-buy '((buys 2) (supply market copper village) (coins 6))) '(buy market))
(test (try-buy '((buys 2) (supply village silver) (coins 4))) '(buy village))

;; ----------------------------------------

(run-player try-action try-buy)
