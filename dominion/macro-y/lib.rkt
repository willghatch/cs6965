#lang racket/base

(require (for-syntax
          syntax/parse
          racket/base
          ))



(struct gamestate
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



;play	 	=	 	(act mine treasure treasure)
; 	 	|	 	(add treasure) ; adds coins
; 	 	|	 	(buy card)
; 	 	|	 	(clean) ; ending a turn with an empty hand
; 	 	|	 	(clean card) ; card in hand is exposed
