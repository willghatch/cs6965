#lang racket/base

(require "card.rkt")
(require "cards.rkt")
(require "gamestate.rkt")
(require racket/match)
(require threading)
(require lens)
(require racket/list)

;play	 	=	 	(act mine treasure treasure)
; 	 	|	 	(add treasure) ; adds coins
; 	 	|	 	(buy card)
; 	 	|	 	(clean) ; ending a turn with an empty hand
; 	 	|	 	(clean card) ; card in hand is exposed

(define (valid-play? state play)
  (with-handlers ([(λ _ #t) (λ _ #f)])
    (match play
      [(list 'act action rest ...)
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
    [(list 'act action rest ...)
     (apply (card-action (card-ref action))
            (cons (~> state
                      (play-from-hand _ (card-ref action))
                      (lens-transform gamestate-actions-lens
                                      _
                                      sub1))
                  rest))]
    [(list 'add treasure)
     (~> state
         (play-from-hand _ (card-ref treasure))
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


(define (parse-state state-sexp)
  (gamestate
   (rest (first state-sexp))
   (map card-ref (rest (second state-sexp)))
   (map card-ref (rest (third state-sexp)))

   (second (fourth state-sexp))
   (second (fifth state-sexp))
   (second (sixth state-sexp))

   (map card-ref (rest (seventh state-sexp)))
   (map card-ref (rest (eighth state-sexp)))
   (map card-ref (rest (ninth state-sexp)))
   (map card-ref (rest (tenth state-sexp)))
   ))

#;(define (parse-state state-sexp)
  (match
      [(list (list 'players names ...)
             (list 'supply supply ...)
             (list 'trash trash ...)

             (list 'actions actions)
             (list 'buys buys)
             (list 'coins coins)

             (list 'deck deck ...)
             (list 'hand hand ...)
             (list 'plays plays ...)
             (list 'discards discards ...))
       (gamestate names
                  (map card-ref supply)
                  (map card-ref trash)

                  actions
                  buys
                  coins

                  (map card-ref deck)
                  (map card-ref hand)
                  (map card-ref plays)
                  (map card-ref discards))]))

(define (state->sexp state)
  (list
   (cons 'players (gamestate-players state))
   (cons 'supply (map card-name (gamestate-supply state)))
   (cons 'trash (map card-name (gamestate-trash state)))

   (list 'actions (gamestate-actions state))
   (list 'buys (gamestate-buys state))
   (list 'coins (gamestate-coins state))

   (cons 'deck (map card-name (gamestate-deck state)))
   (cons 'hand (map card-name (gamestate-hand state)))
   (cons 'plays (map card-name (gamestate-plays state)))
   (cons 'discards (map card-name (gamestate-discards state)))
   ))


(module+ test

  (require rackunit)

  (define test-state
    '((players "bob" "sally")
      (supply gold gold silver copper mine mine duchy province estate)
      (trash gold gold silver copper mine mine duchy province estate)
      (actions 2)
      (buys 3)
      (coins 7)
      (deck gold gold silver copper mine mine duchy province estate)
      (hand gold gold silver copper mine mine duchy province estate)
      (plays gold gold silver copper mine mine duchy province estate)
      (discards gold gold silver copper mine mine duchy province estate)
      ))

  (check-equal? test-state (state->sexp (parse-state test-state)))

  
  )
