#lang racket/base

(provide (all-defined-out))

(require lens)
(require threading)

(struct/lens gamestate
  ;; these are all lists except actions buys and coins
  (players
   supply
   trash

   actions
   buys
   coins

   deck
   hand
   plays
   discards)
  #:transparent)

;state	 	=	 	
;((players name ...) ; in order, current first
; (supply card ...)
; (trash card ...) ; in order, top to bottom
; 
; (actions number) ; actions remaining in turn
; (buys number) ; buys remaining in turn
; (coins number) ; coins available for buys
; 
; (deck card ...) ; not in draw order
; (hand card ...)
; (plays card ...)
; (discards card ...))

(define-syntax-rule (define-card-mover name from-lens to-lens)
  (define (name state card)
    (~> state
        (lens-transform from-lens
                        _
                        (λ (from) (remove card from)))
        (lens-transform to-lens
                        _
                        (λ (to) (cons card to))))))

(define-card-mover discard-from-hand gamestate-hand-lens gamestate-discards-lens)
(define-card-mover trash-from-hand gamestate-hand-lens gamestate-trash-lens)
(define-card-mover discard-from-supply gamestate-supply-lens gamestate-discards-lens)
(define-card-mover play-from-hand gamestate-hand-lens gamestate-plays-lens)

