#lang racket/base

(require "lib.rkt")
(require "card.rkt")
(require "cards.rkt")
(require "gamestate.rkt")
(require racket/list)
(require racket/match)

(define (play-loop)
  (let ([message (read)])
    (match message
      [(list moved name play) (void)]
      [(list move state)
       (do-play (parse-state state))]))
  (play-loop))


(define (do-play state)
  (printf "~a~n" (decide-play state))
  (flush-output))

;; I don't remember offhand how to introduce these things non-hygienically...
(define-syntax-rule (define/state (name state)
                      body ...)
  (define (name state)
    (let ([players (gamestate-players state)]
          [supply (gamestate-supply state)]
          [actions (gamestate-actions state)]
          [buys (gamestate-buys state)]
          [coins (gamestate-coins state)]
          [deck (gamestate-deck state)]
          [hand (gamestate-hand state)])
      body ...)))

(define (decide-play state)
  (cond
    [(and (< 0 (gamestate-actions state))
          (member mine (gamestate-hand state)))
     (decide-action state)]
    [(member gold (gamestate-hand state))
     '(add gold)]
    [(member silver (gamestate-hand state))
     '(add silver)]
    [(member copper (gamestate-hand state))
     '(add copper)]
    [(< 0 (gamestate-buys state))
     (decide-buy state)]
    [(empty? (gamestate-hand state))
     '(clean)]
    [else
     `(clean ,(card-name (first  (gamestate-hand state))))]
    ))

(define (decide-action state)
  (cond
    [(member mine (gamestate-hand state))
     (cond
       [(and (member copper (gamestate-hand state))
             (member silver (gamestate-supply state)))
        (list 'act 'mine 'copper 'silver)]
       [(and (member silver (gamestate-hand state))
             (member gold (gamestate-supply state)))
        (list 'act 'mine 'silver 'gold)]
       [else '(clean mine)])]
    [else `(clean ,(first (gamestate-hand state)))]
    ))

(define (decide-buy state)
  (let* ([availables (filter (λ (c) (>= (gamestate-coins state)
                                        (card-cost c)))
                             (gamestate-supply state))]
         [expensive-est (first (reverse
                                (sort availables
                                      <
                                      #:key card-cost
                                      #:cache-keys? #t)))])
    `(buy ,(card-name expensive-est))
    ))



(module+ main
  (play-loop))

