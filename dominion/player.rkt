#lang racket/base

(require "lib.rkt")
(require "card.rkt")
(require "cards.rkt")
(require "gamestate.rkt")
(require racket/list)
(require racket/match)
(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

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

;; non-hygienically introduce identifiers for the parts of state
(define-syntax (define/state stx)
  (syntax-case stx ()
    [(d (name state) body ...)
     (with-syntax ([players (datum->syntax #'d 'players)]
                   [supply (datum->syntax #'d 'supply)]
                   [actions (datum->syntax #'d 'actions)]
                   [buys (datum->syntax #'d 'buys)]
                   [coins (datum->syntax #'d 'coins)]
                   [deck (datum->syntax #'d 'deck)]
                   [hand (datum->syntax #'d 'hand)]
                   )
       #'(define (name state)
           (let ([players (gamestate-players state)]
                 [supply (gamestate-supply state)]
                 [actions (gamestate-actions state)]
                 [buys (gamestate-buys state)]
                 [coins (gamestate-coins state)]
                 [deck (gamestate-deck state)]
                 [hand (gamestate-hand state)])
             body ...)))]
    [else (raise-syntax-error "define/state - bad syntax")]))

(define/state (decide-play state)
  (cond
    [(and (< 0 actions)
          (member mine hand))
     (decide-action state)]
    [(member gold hand)
     '(add gold)]
    [(member silver hand)
     '(add silver)]
    [(member copper hand)
     '(add copper)]
    [(< 0 buys)
     (decide-buy state)]
    [(empty? hand)
     '(clean)]
    [else
     `(clean ,(card-name (first  (gamestate-hand state))))]
    ))

(define/state (decide-action state)
  (cond
    [(member mine hand)
     (cond
       [(and (member copper hand)
             (member silver supply))
        (list 'act 'mine 'copper 'silver)]
       [(and (member silver hand)
             (member gold supply))
        (list 'act 'mine 'silver 'gold)]
       [else '(clean mine)])]
    [else `(clean ,(first hand))]
    ))

(define/state (decide-buy state)
  (let* ([availables (filter (λ (c) (>= coins
                                        (card-cost c)))
                             supply)]
         [expensive-est (first (reverse
                                (sort availables
                                      <
                                      #:key card-cost
                                      #:cache-keys? #t)))])
    `(buy ,(card-name expensive-est))
    ))



(module+ main
  (play-loop))

