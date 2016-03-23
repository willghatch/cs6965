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


(define p1.M (struct-copy player p1
                          [hand '(mine silver copper estate estate)]))
(define gM (struct-copy game g1
                        [players (list p1.M p2)]))

(define p1.C (struct-copy player p1
                          [hand '(cellar silver copper estate estate)]))
(define gC (struct-copy game g1
                        [players (list p1.C p2)]))

(define p1.W (struct-copy player p1
                          [hand '(workshop silver copper estate estate)]))
(define gW (struct-copy game g1
                        [players (list p1.W p2)]
                        [supply '(smithy copper gold silver mine)]))

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
;;  Draw cards to get player's hand up to `up-to' cards,
;;  shuffling discards to deck as needed. The total
;;  number of cards in the deck plus hand plus discards
;;  can be less than `up-to', in whch case all cards
;;  end up in the player's hand.
(define (draw-cards p [up-to 5])
  (cond
    [(>= (length (player-hand p)) up-to)
     p]
    [(empty? (player-deck p))
     (if (empty? (player-discards p))
         ;; ran out of cards to draw or shuffle:
         p
         ;; shuffle and try again:
         (draw-cards (struct-copy player p
                                  [deck (shuffle (player-discards p))]
                                  [discards '()])
                     up-to))]
    [else
     (draw-cards (struct-copy player p
                              [deck (cdr (player-deck p))]
                              [hand (cons (car (player-deck p))
                                          (player-hand p))])
                 up-to)]))

(test (draw-cards p1) p1)
(test (draw-cards p1 4) p1)
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
(test (draw-cards (player 'px 
                          '(silver)
                          '(gold)
                          '(estate estate estate estate))
                  7)
      (player 'px               
              '()
              '(estate estate estate estate silver gold)
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
  (trade 'mine 3 g t old new #t))

;; remodel : game turn treasure treasure -> (values game turn)
(define (remodel g t old new)
  (trade 'remodel 2 g t old new #f))

;; trade : card number game turn treasure treasure -> (values game turn)
(define (trade which delta g t old new to-hand?)
  (define p (car (game-players g)))
  (check-in-hand p which)
  (check-plays t which)
  (unless (member old (player-hand p))
    (error which "old card is not in hand: ~e" old))
  (unless (member new (game-supply g))
    (error which "new card is not in supply: ~e" new))
  (unless ((cost-of new) . <= . (+ delta (cost-of old)))
    (error which "new card is too expensive: ~e" new))
  (define pre-p (struct-copy player p
                             [hand (remove which 
                                           (remove old (player-hand p)))]))
  (define new-p (if to-hand?
                    (struct-copy player pre-p
                                 [hand (cons new (player-hand pre-p))])
                    (struct-copy player pre-p
                                 [discards (cons new (player-discards pre-p))])))
  (values (struct-copy game (replace-player g new-p)
                       [supply (remove new (game-supply g))]
                       [trash (cons old (game-trash g))])
          (struct-copy turn t
                       [plays (cons which (turn-plays t))]
                       [actions (sub1 (turn-actions t))])))

(test/values (mine gM (turn '() 1 1 0) 'copper 'copper)
             (struct-copy game gM
                          [players (list (struct-copy player p1
                                                      [hand '(copper silver estate estate)])
                                         p2)]
                          [supply '(gold silver mine)]
                          [trash '(copper copper)])
             (turn '(mine) 0 1 0))
(test/values (mine gM (turn '() 1 1 0) 'copper 'silver)
             (struct-copy game gM
                          [players (list (struct-copy player p1
                                                      [hand '(silver silver estate estate)])
                                         p2)]
                          [supply '(copper gold mine)]
                          [trash '(copper copper)])
             (turn '(mine) 0 1 0))
(test/values (mine gM (turn '() 1 1 0) 'silver 'gold)
             (struct-copy game gM
                          [players (list (struct-copy player p1
                                                      [hand '(gold copper estate estate)])
                                         p2)]
                          [supply '(copper silver mine)]
                          [trash '(silver copper)])
             (turn '(mine) 0 1 0))
(test/values (mine gM (turn '() 1 1 0) 'silver 'copper)
             (struct-copy game gM
                          [players (list (struct-copy player p1
                                                      [hand '(copper copper estate estate)])
                                         p2)]
                          [supply '(gold silver mine)]
                          [trash '(silver copper)])
             (turn '(mine) 0 1 0))
(test/exn (mine gM (turn '() 0 0 0) 'copper 'copper) "no actions allowed")
(test/exn (mine (struct-copy game gM [supply '()])
                (turn '() 1 1 0) 'copper 'copper)
          "not in supply")
(test/exn (mine gM (turn '() 1 1 0) 'copper 'gold) "too expensive")

(test/exn (remodel gM (turn '() 1 1 0) 'copper 'silver)
          "no such card in hand")
(test/exn (remodel (replace-player gM
                                   (struct-copy player p1.M
                                                [hand '(remodel silver copper estate estate)]))
                   (turn '() 1 1 0) 'copper 'silver)
          "too expensive")
(test/values (remodel (replace-player (struct-copy game gM
                                                   [supply '(copper estate gold silver mine)])
                                      (struct-copy player p1.M
                                                   [hand '(remodel silver copper estate estate)]))
                      (turn '() 1 1 0) 'copper 'estate)
             (struct-copy game gM
                          [players (list (struct-copy player p1
                                                      [hand '(silver estate estate)]
                                                      [discards '(estate)])
                                         p2)]
                          [supply '(copper gold silver mine)]
                          [trash '(copper copper)])
             (turn '(remodel) 0 1 0))

;; ----------------------------------------

;; cellar : game turn cards -> (values game turn)
(define (cellar g t cards)
  (define p (car (game-players g)))
  (check-in-hand p 'cellar)
  (check-plays t 'cellar)
  (define remain
    (for/fold ([remain (remq 'cellar (player-hand p))]) ([c cards])
      (unless (memq c remain)
        (error 'cellar "card is not available: ~e in remaining hand: ~e" c remain))
      (remq c remain)))
  (define new-p (draw-cards
                 (struct-copy player p
                              [hand remain]
                              [discards (append cards (player-discards p))])
                 (+ (length remain) (length cards))))
  (values (struct-copy game (replace-player g new-p))
          (struct-copy turn t
                       [plays (cons 'cellar (turn-plays t))])))

(test/exn (cellar g1 (turn '() 1 1 0) '()) "no such card in hand")
(test/exn (cellar gC (turn '() 0 1 0) '()) "no actions allowed")
(test/values (cellar gC (turn '() 1 1 0) '())
             (replace-player g1 (struct-copy player p1.C
                                             [hand '(silver copper estate estate)]))
             (turn '(cellar) 1 1 0))
(test/values (cellar gC (turn '() 1 1 0) '(copper estate))
             (replace-player g1 (struct-copy player p1.C
                                             [deck '(copper copper estate)]
                                             [hand '(copper copper silver estate)]
                                             [discards '(copper estate)]))
             (turn '(cellar) 1 1 0))
(test/values (cellar gC (turn '() 1 1 0) '(estate estate))
             (replace-player g1 (struct-copy player p1.C
                                             [deck '(copper copper estate)]
                                             [hand '(copper copper silver copper)]
                                             [discards '(estate estate)]))
             (turn '(cellar) 1 1 0))
(test/exn (cellar gC (turn '() 1 1 0) '(copper copper))
          "card is not available")
(test/exn (cellar gC (turn '() 1 1 0) '(cellar))
          "card is not available")

;; ----------------------------------------

;; market : game turn -> (values game turn)
(define (market g t)
  (up g t 'market #:cards 1 #:actions 1 #:buys 1 #:coins 1))

;; smithy : game turn -> (values game turn)
(define (smithy g t)
  (up g t 'smithy #:cards 3))

;; village : game turn -> (values game turn)
(define (village g t)
  (up g t 'village #:cards 1 #:actions 2))

;; woodcutter : game turn -> (values game turn)
(define (woodcutter g t)
  (up g t 'woodcutter #:buys 1 #:coins 2))

;; militia : game turn -> (values game turn)
(define (militia g t)
  (up g t 'militia #:coins 2))

;; moat : game turn -> (values game turn)
(define (moat g t)
  (up g t 'moat #:cards 2))

;; up : game turn which number number number number -> (values game turn)
(define (up g t which 
            #:cards [dcards 0] 
            #:actions [dactions 0]
            #:buys [dbuys 0]
            #:coins [dcoins 0])
  (define p (car (game-players g)))
  (check-in-hand p which)
  (check-plays t which)
  (define new-p (draw-cards
                 (struct-copy player p
                              [hand (remq which (player-hand p))])
                 (+ -1 dcards (length (player-hand p)))))
  (values (struct-copy game (replace-player g new-p))
          (struct-copy turn t
                       [plays (cons which (turn-plays t))]
                       [actions (+ -1 dactions (turn-actions t))]
                       [buys (+ dbuys (turn-buys t))]
                       [coins (+ dcoins (turn-coins t))])))

(test/exn (up gM (turn '() 1 1 0) 'market) "no such card in hand")
(test/exn (up gM (turn '() 0 1 0) 'mine) "no actions allowed")
(test/values (up gM (turn '() 1 1 0) 'mine)
             (replace-player gM (struct-copy player p1.M
                                             [hand '(silver copper estate estate)]))
             (turn '(mine) 0 1 0))
(test/values (up gM (turn '() 1 1 2) 'mine
                 #:cards 1
                 #:actions 2
                 #:buys 3
                 #:coins 4)
             (replace-player gM (struct-copy player p1.M
                                             [deck '(copper copper copper estate)]
                                             [hand '(copper silver copper estate estate)]))
             (turn '(mine) 2 4 6))

;; ----------------------------------------

;; workshop : game turn card -> (values game turn)
(define (workshop g t new)
  (define p (car (game-players g)))
  (check-in-hand p 'workshop)
  (check-plays t 'workshop)
  (unless (member new (game-supply g))
    (error 'workshop "new card is not in supply: ~e" new))
  (unless ((cost-of new) . <= . 4)
    (error 'workshop "new card is too expensive: ~e" new))
  (define new-p (struct-copy player p
                             [hand (remove 'workshop (player-hand p))]
                             [discards (cons new (player-discards p))]))
  (values (struct-copy game (replace-player g new-p)
                       [supply (remove new (game-supply g))])
          (struct-copy turn t
                       [plays (cons 'workshop (turn-plays t))]
                       [actions (sub1 (turn-actions t))])))

(test/exn (workshop g1 (turn '() 1 1 0) 'silver) "no such card in hand")
(test/exn (workshop gW (turn '() 0 1 0) 'silver) "no actions allowed")
(test/exn (workshop gW (turn '() 1 1 0) 'mine) "too expensive")
(test/exn (workshop gW (turn '() 1 1 0) 'village) "not in supply")
(test/values (workshop gW (turn '() 1 1 0) 'smithy)
             (struct-copy game (replace-player gW
                                               (struct-copy player p1.W 
                                                            [hand '(silver copper estate estate)]
                                                            [discards '(smithy)]))
                          [supply (remq 'smithy (game-supply gW))])
             (turn '(workshop) 0 1 0))

;; ----------------------------------------

;; add : game turn treasure -> (values game turn)
(define (add g t card)
  (define p (car (game-players g)))
  (check-in-hand p card)
  (unless (positive? (turn-buys t))
    (error 'buy "no buys allowed"))
  (define new-p (struct-copy player p
                             [hand (remove card (player-hand p))]))
  (values (replace-player g new-p)
          (struct-copy turn t
                       [plays (cons card (turn-plays t))]
                       [actions 0]
                       [coins (+ (turn-coins t) (treasure-value card))])))

(test/exn (add gM (turn '() 0 0 0) 'gold) "no such card in hand")
(test/exn (add gM (turn '() 0 0 0) 'copper) "no buys allowed")
(test/values (add gM (turn '() 1 2 0) 'copper)
             (replace-player gM (player
                                 'p1
                                 '(copper copper copper copper estate)
                                 '(mine silver estate estate)
                                 '()))
             (turn '(copper) 0 2 1))
(test/values (add gM (turn '() 1 2 0) 'silver)
             (replace-player gM (player
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

(test/values (buy gM (turn '() 2 1 0) 'copper)
             (game (list
                    (struct-copy player p1.M
                                 [discards '(copper)])
                    p2)
                   '(gold silver mine)
                   '(copper))
             (turn '() 0 0 0))
(test/values (buy gM (turn '() 0 1 3) 'silver)
             (game (list
                    (struct-copy player p1.M
                                 [discards '(silver)])
                    p2)
                   '(copper gold mine)
                   '(copper))
             (turn '() 0 0 0))
(test/values (buy gM (turn '() 0 3 6) 'mine)
             (game (list
                    (struct-copy player p1.M
                                 [discards '(mine)])
                    p2)
                   '(copper gold silver)
                   '(copper))
             (turn '() 0 2 1))
(test/exn (buy gM (turn '() 0 0 0) 'copper) "no buys allowed")
(test/exn (buy gM (turn '() 0 1 0) 'silver) "too expensive")
(test/exn (buy gM (turn '() 0 1 0) 'duchy) "card not in supply")
  
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
   [`(act cellar ,(and (? card?) cards) ...)
    (cellar g t cards)]
   [`(act market) (market g t)]
   [`(act remodel ,(and (? card?) old) ,(and (? card?) new))
    (remodel g t old new)]
   [`(act smithy) (smithy g t)]
   [`(act village) (village g t)]
   [`(act woodcutter) (woodcutter g t)]
   [`(act workshop ,(and (? card?) card))
    (workshop g t card)]
   [`(act militia) (militia g t)]
   [`(act moat) (moat g t)]
   [`(add ,(and (? treasure?) card)) (add g t card)]
   [`(buy ,(and (? card?) card)) (buy g t card)]
   [`(clean) (values (clean g t #f) #f)]
   [`(clean ,(and (? card?) top-card)) (values (clean g t top-card) #f)]
   [else (error 'move "bad play: ~e" play)]))
