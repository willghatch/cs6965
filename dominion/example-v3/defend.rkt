#lang racket
(require "cards.rkt"
         "move.rkt"
         "test.rkt")

(provide attack?
         prune-defense
         defend)

;; attack? : play -> boolean
(define (attack? play)
  (match play
    [`(act militia) #t]
    [else #f]))

(test (attack? '(act militia)) #t)
(test (attack? '(buy militia)) #f)

;; prune-defense : defense -> defense
;;  Drops information from a defense that should not be broadcast
(define (prune-defense defense)
  (case (car defense)
    [(discard) (take defense (min (length defense) 2))]
    [else defense]))

(test (prune-defense '(moat)) '(moat))
(test (prune-defense '(discard)) '(discard))
(test (prune-defense '(discard copper estate)) '(discard copper))

;; ----------------------------------------

;; moat : player -> player
;;  Checks that a moat defense is valid and returns the player unchanged
(define (moat p)
  (unless (member 'moat (player-hand p))
    (error 'moat "no such card in hand for defense"))
  p)

(define p1 (player 'p1
                   '(gold copper)
                   '(copper copper moat estate estate)
                   '()))

(test/exn (moat (player 'p1
                        '(moat copper)
                        '(copper copper copper estate estate)
                        '(moat))) 
          "no such card in hand")
(test (moat p1) p1)

;; ----------------------------------------

;; discard : player list-of-card -> player
(define (discard p cards)
  (define new-hand
    (for/fold ([hand (player-hand p)]) ([c (in-list cards)])
      (unless (member c hand)
        (error 'discard "no such card in hand to discard: ~e" c))
      (remq c hand)))
  (unless (or (= (length new-hand) 3)
              (and (< (length (player-hand p)) 3)
                   (null? cards)))
    (error 'discard "remaining hand is not of size 3: ~e" new-hand))
  (struct-copy player p 
               [hand new-hand]
               [discards (append cards (player-discards p))]))

(test (discard p1 '(copper copper)) (player 'p1
                                            '(gold copper)
                                            '(moat estate estate)
                                            '(copper copper)))
(test (discard p1 '(estate copper)) (player 'p1
                                            '(gold copper)
                                            '(copper moat estate)
                                            '(estate copper)))
(define p2 (player 'p2
                   '(gold copper)
                   '(copper moat estate)
                   '()))
(test (discard p2 '()) p2)
(test/exn (discard p1 '(duchy)) "no such card in hand")
(test/exn (discard p1 '(estate)) "remaining hand is not of size 3")
(test/exn (discard p1 '()) "remaining hand is not of size 3")
(test/exn (discard p1 '(estate copper copper)) "remaining hand is not of size 3")

;; ----------------------------------------

;; defend : player defense -> player
(define (defend p defense)
  (match defense
    [`(moat) (moat p)]
    [`(discard ,(and (? card?) cards) ...) (discard p cards)]
    [else (error 'defend "bad defense: ~e" defense)]))
