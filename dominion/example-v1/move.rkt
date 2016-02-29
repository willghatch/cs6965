#lang racket
(require "cards.rkt"
         "test.rkt")

(provide (struct-out game)
         (struct-out player)
         (struct-out turn)
         game+turn->state
         
         draw-cards
         
         mine
         buy
         add
         mine
         clean
         
         move)

;; A game is
;;  (game list-of-player list-of-card list-of-card)
;; First player in list is the current player
(struct game (players supply trash)
        #:transparent)

;; A player is
;;  (player symbol list-of-card list-of-card list-of-card)
(struct player (name deck hand discards)
        #:transparent)

;; A turn is
;;  (turn list-of-card number number number)
(struct turn (plays actions buys coins)
        #:transparent)

(define p1 (player 'p1
                   '(copper copper copper copper estate)
                   '(copper copper copper estate estate)
                   '()))
(define p2 (player 'p2
                   '(copper copper copper estate estate)
                   '()
                   '(copper copper copper copper estate mine)))
(define g1 (game (list p1 p2)
                 '(copper gold silver mine)
                 '(copper)))


(define p1.2 (struct-copy player p1
                          [hand '(mine silver copper estate estate)]))
(define g2 (struct-copy game g1
                        [players (list p1.2 p2)]))

;; ----------------------------------------

;; sort-deck : list-of-cards -> list-of-cards
;;  We sort a deck so that a player can't rely on the order.
;;  Sorting rather than shuffling makes testing easier.
(define (sort-deck d)
  (sort d (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))

(test (sort-deck '()) '())
(test (sort-deck '(estate gold)) '(estate gold))
(test (sort-deck '(gold estate)) '(estate gold))

;; game+turn->state : game turn -> game-state
(define (game+turn->state g t)
  (define p (car (game-players g)))
  `((players ,@(map player-name (game-players g)))
    (supply ,@(game-supply g))
    (trash ,@(game-trash g))
    
    (actions ,(turn-actions t))
    (buys ,(turn-buys t))
    (coins ,(turn-coins t))
    
    (deck ,@(sort-deck (player-deck p)))
    (hand ,@(player-hand p))
    (plays ,@(turn-plays t))
    (discards ,@(player-discards p))))

(test (game+turn->state g1 (turn '() 2 1 0))
      '((players p1 p2)
        (supply copper gold silver mine)
        (trash copper)
        (actions 2)
        (buys 1)
        (coins 0)
        (deck copper copper copper copper estate)
        (hand copper copper copper estate estate)
        (plays)
        (discards)))

;; ----------------------------------------

;; draw-cards : player -> player
;;  Draw cards to get player's hand up to 5 cards,
;;  shuffling discards to deck as needed. The total
;;  number of cards in the deck plus hand plus discards
;;  must be at least 5.
(define (draw-cards p)
  (cond
    [(= (length (player-hand p)) 5)
     p]
    [(empty? (player-deck p))
     (draw-cards (struct-copy player p
                              [deck (shuffle (player-discards p))]
                              [discards '()]))]
    [else
     (draw-cards (struct-copy player p
                              [deck (cdr (player-deck p))]
                              [hand (cons (car (player-deck p))
                                          (player-hand p))]))]))

(test (draw-cards p1) p1)
(test (draw-cards (player 'px 
                          '(copper silver gold mine silver)
                          '()
                          '(estate)))
      (player 'px               
              '()
              '(silver mine gold silver copper)
              '(estate)))
(test (draw-cards (player 'px 
                          '(silver)
                          '(gold)
                          '(estate estate estate estate)))
      (player 'px               
              '(estate)
              '(estate estate estate silver gold)
              '()))

;; ----------------------------------------

(define (check-in-hand p c)
  (unless (member c (player-hand p))
    (error c "no such card in hand: ~e" (player-hand p))))

(define (check-plays t what)
  (unless (positive? (turn-actions t))
    (error what "no actions allowed")))

(define (replace-player g new-p)
  (struct-copy game g
               [players (cons new-p (cdr (game-players g)))]))
                       
;; ----------------------------------------

;; mine : game turn treasure treasure -> (values game turn)
(define (mine g t old new)
  (define p (car (game-players g)))
  (check-in-hand p 'mine)
  (check-plays t 'mine)
  (unless (member old (player-hand p))
    (error 'mine "old card is not in hand: ~e" old))
  (unless (member new (game-supply g))
    (error 'mine "new card is not in supply: ~e" new))
  (unless ((cost-of new) . <= . (+ 3 (cost-of old)))
    (error 'mine "new card is too expensive: ~e" new))
  (define new-p (struct-copy player p
                             [hand (cons new
                                         (remove 'mine 
                                                 (remove old (player-hand p))))]))
  (values (struct-copy game (replace-player g new-p)
                       [supply (remove new (game-supply g))]
                       [trash (cons old (game-trash g))])
          (struct-copy turn t
                       [plays (cons 'mine (turn-plays t))]
                       [actions (sub1 (turn-actions t))])))

(test/values (mine g2 (turn '() 1 1 0) 'copper 'copper)
             (struct-copy game g2
                          [players (list (struct-copy player p1
                                                      [hand '(copper silver estate estate)])
                                         p2)]
                          [supply '(gold silver mine)]
                          [trash '(copper copper)])
             (turn '(mine) 0 1 0))
(test/values (mine g2 (turn '() 1 1 0) 'copper 'silver)
             (struct-copy game g2
                          [players (list (struct-copy player p1
                                                      [hand '(silver silver estate estate)])
                                         p2)]
                          [supply '(copper gold mine)]
                          [trash '(copper copper)])
             (turn '(mine) 0 1 0))
(test/values (mine g2 (turn '() 1 1 0) 'silver 'gold)
             (struct-copy game g2
                          [players (list (struct-copy player p1
                                                      [hand '(gold copper estate estate)])
                                         p2)]
                          [supply '(copper silver mine)]
                          [trash '(silver copper)])
             (turn '(mine) 0 1 0))
(test/values (mine g2 (turn '() 1 1 0) 'silver 'copper)
             (struct-copy game g2
                          [players (list (struct-copy player p1
                                                      [hand '(copper copper estate estate)])
                                         p2)]
                          [supply '(gold silver mine)]
                          [trash '(silver copper)])
             (turn '(mine) 0 1 0))
(test/exn (mine g2 (turn '() 0 0 0) 'copper 'copper) "no actions allowed")
(test/exn (mine (struct-copy game g2 [supply '()])
                (turn '() 1 1 0) 'copper 'copper)
          "not in supply")
(test/exn (mine g2 (turn '() 1 1 0) 'copper 'gold) "too expensive")

;; ----------------------------------------

;; add : game turn treasure -> (values game turn)
(define (add g t card)
  (define p (car (game-players g)))
  (check-in-hand p card)
  (define new-p (struct-copy player p
                             [hand (remove card (player-hand p))]))
  (values (replace-player g new-p)
          (struct-copy turn t
                       [plays (cons card (turn-plays t))]
                       [actions 0]
                       [coins (+ (turn-coins t) (treasure-value card))])))

(test/exn (add g2 (turn '() 0 0 0) 'gold) "no such card in hand")
(test/values (add g2 (turn '() 1 2 0) 'copper)
             (replace-player g2 (player
                                 'p1
                                 '(copper copper copper copper estate)
                                 '(mine silver estate estate)
                                 '()))
             (turn '(copper) 0 2 1))
(test/values (add g2 (turn '() 1 2 0) 'silver)
             (replace-player g2 (player
                                 'p1
                                 '(copper copper copper copper estate)
                                 '(mine copper estate estate)
                                 '()))
             (turn '(silver) 0 2 2))

;; ----------------------------------------

;; buy : game turn card -> (values game turn)
(define (buy g t card)
  (define p (car (game-players g)))
  (unless (positive? (turn-buys t))
    (error 'buy "no buys allowed"))
  (unless (member card (game-supply g))
    (error 'buy "card not in supply: ~e" card))
  (unless ((cost-of card) . <= . (turn-coins t))
    (error 'buy "too expensive: ~e" card))
  (define new-p (struct-copy player p
                             [discards (cons card (player-discards p))]))
  (values (struct-copy game (replace-player g new-p)
                       [supply (remove card (game-supply g))])
          (struct-copy turn t
                       [actions 0]
                       [buys (sub1 (turn-buys t))]
                       [coins (- (turn-coins t) (cost-of card))])))

(test/values (buy g2 (turn '() 2 1 0) 'copper)
             (game (list
                    (struct-copy player p1.2
                                 [discards '(copper)])
                    p2)
                   '(gold silver mine)
                   '(copper))
             (turn '() 0 0 0))
(test/values (buy g2 (turn '() 0 1 3) 'silver)
             (game (list
                    (struct-copy player p1.2
                                 [discards '(silver)])
                    p2)
                   '(copper gold mine)
                   '(copper))
             (turn '() 0 0 0))
(test/values (buy g2 (turn '() 0 3 6) 'mine)
             (game (list
                    (struct-copy player p1.2
                                 [discards '(mine)])
                    p2)
                   '(copper gold silver)
                   '(copper))
             (turn '() 0 2 1))
(test/exn (buy g2 (turn '() 0 0 0) 'copper) "no buys allowed")
(test/exn (buy g2 (turn '() 0 1 0) 'silver) "too expensive")
(test/exn (buy g2 (turn '() 0 1 0) 'duchy) "card not in supply")
  
;; ----------------------------------------

;; clean : game turn card-or-false -> game
(define (clean g t top-card)
  (define p (car (game-players g)))
  (cond
    [top-card (unless (member top-card (player-hand p))
                (error 'clean "top card for clean up is not in hand: ~e" top-card))]
    [else (unless (empty? (player-hand p))
            (error 'clean "need a top card for non-empty hand: ~e" (player-hand p)))])
  (define new-p (draw-cards
                 (struct-copy player p 
                              [hand '()]
                              [discards (append (player-hand p)
                                                (turn-plays t)
                                                (player-discards p))])))
  (struct-copy game g 
               [players (append (cdr (game-players g)) (list new-p))]))

(test/exn (clean g1 (turn '(mine) 0 0 0) #f) "need a top card")
(test/exn (clean g1 (turn '(mine) 0 0 0) 'gold) "top card .* is not in hand")
(test (clean g1 (turn '(mine) 0 0 0) 'copper)
      (game (list p2 (player 
                      'p1
                      '()
                      '(estate copper copper copper copper)                      
                      '(copper copper copper estate estate mine)))
            '(copper gold silver mine)
            '(copper)))

;; ----------------------------------------

;; move : game turn play -> (values game turn-or-#f)
;;  Checks that `play' matches the grammar, and then
;;  checks whether it is a valid play and implements
;;  its effect.
(define (move g t play)
 (match play
   [`(act mine ,(and (? treasure?) old) ,(and (? treasure?) new))
    (mine g t old new)]
   [`(add ,(and (? treasure?) card)) (add g t card)]
   [`(buy ,(and (? card?) card)) (buy g t card)]
   [`(clean) (values (clean g t #f) #f)]
   [`(clean ,(and (? card?) top-card)) (values (clean g t top-card) #f)]
   [else (error 'move "bad play: ~e" play)]))
