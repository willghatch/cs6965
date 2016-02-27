#lang racket/base

(require "card.rkt")
(require "cards.rkt")
(require racket/match)

;play	 	=	 	(act mine treasure treasure)
; 	 	|	 	(add treasure) ; adds coins
; 	 	|	 	(buy card)
; 	 	|	 	(clean) ; ending a turn with an empty hand
; 	 	|	 	(clean card) ; card in hand is exposed

(define (valid-play? state play)
  (with-handlers ([(λ _ #t) (λ _ #f)])
    (match play
      [(list 'act action . rest)
       (and
        (< 0 (gamestate-actions state))
        (member (card-ref action) (gamestate-hand state))
        ((card-action-validator (card-ref action)) state play))]
      [(list 'add treasure)
       (member (card-ref treasure) (gamestate-hand state))]
      [(list 'buy card)
       (and
        (>= (gamestate-coins state)
            (card-cost (card-ref card)))
        (< 0 (gamestate-buys state))
        (member (card-ref card) (gamestate-supply state)))]
      [(list 'clean card)
       (member (card-ref card) (gamestate-hand state))]
      [(list 'clean)
       (null? (gamestate-hand state))]
      [else #f])))

(define (do-play state play)
  (match play
    [(list 'act action . rest)
     (apply (card-action (card-ref action))
            (cons (~> state
                      (plays-from-hand _ (card-ref action))
                      (lens-transform gamestate-actions-lens
                                      _
                                      sub1))
                  rest))]
    [(list 'add treasure)
     (~> state
         (plays-from-hand _ (card-ref treasure))
         (lens-transform gamestate-coins-lens
                         _
                         (λ (c) (+ c (card-coin-value (card-ref treasure))))))]
    [(list 'buy card)
     (~> state
         (discard-from-supply _ (card-ref card))
         (lens-transform gamestate-coins-lens
                         _
                         (λ (c) (- c (card-cost (card-ref card))))))]
    [(list 'clean card)
     (let ([hand-minus-card (remove (card-ref card) (gamestate-hand state))])
       (~> state
           (lens-set gamestate-hand-lens _ '())
           (lens-transform gamestate-discards-lens
                           _
                           (λ (discards) (append hand-minus-card discards)))
           (lens-transform gamestate-discards-lens
                           _
                           (λ (discards) (cons (card-ref card) discards)))))]
    [(list 'clean)
     state]
    ))

(define (game-over? state supply-list)
  (or (not (member province (gamestate-supply state)))
      (< 3
         (apply + (map (λ (card) (if (member card (gamestate-supply state))
                                     0
                                     1))
                       supply-list)))))

;; TODO test who won

;;; TODO - take turns, serialize/deserialize, server, AI
