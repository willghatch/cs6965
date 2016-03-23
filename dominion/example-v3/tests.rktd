;; no mine card in hand
(move
 ((players p1 p2)
  (supply copper gold silver mine)
  (trash copper)
  (actions 2)
  (buys 1)
  (coins 0)
  (deck copper copper copper copper estate)
  (hand copper copper copper estate estate)
  (plays)
  (discards)))
(act mine copper copper)
#f

;; mine card in hand
(move
 ((players p1 p2)
  (supply copper gold silver mine)
  (trash copper)
  (actions 2)
  (buys 1)
  (coins 0)
  (deck copper copper copper copper estate)
  (hand mine copper copper estate estate)
  (plays)
  (discards)))
(act mine copper copper)
#t

;; mine card in hand, but no actions allowed
(move
 ((players p1 p2)
  (supply copper gold silver mine)
  (trash copper)
  (actions 0)
  (buys 1)
  (coins 0)
  (deck copper copper copper copper estate)
  (hand mine copper copper estate estate)
  (plays)
  (discards)))
(act mine copper copper)
#f

;; requested card for mine not in supply
(move
 ((players p1 p2)
  (supply copper gold mine)
  (trash copper)
  (actions 1)
  (buys 1)
  (coins 0)
  (deck copper copper copper copper estate)
  (hand mine copper copper estate estate)
  (plays)
  (discards)))
(act mine copper silver)
#f

;; ok cellar
(move
 ((players p1 p2)
  (supply copper gold mine)
  (trash copper)
  (actions 1)
  (buys 1)
  (coins 0)
  (deck copper copper copper copper estate)
  (hand cellar copper copper estate estate)
  (plays)
  (discards)))
(act cellar copper copper)
#t

;; card listed in cellar not in hand
(move
 ((players p1 p2)
  (supply copper gold mine)
  (trash copper)
  (actions 1)
  (buys 1)
  (coins 0)
  (deck copper copper copper copper estate)
  (hand cellar copper copper estate estate)
  (plays)
  (discards)))
(act cellar copper silver)
#f

;; ok market
(move
 ((players p1 p2)
  (supply copper gold mine)
  (trash copper)
  (actions 1)
  (buys 1)
  (coins 0)
  (deck copper copper copper copper estate)
  (hand cellar copper copper estate market)
  (plays)
  (discards)))
(act market)
#t

;; ok remodel
(move
 ((players p1 p2)
  (supply copper gold mine smithy)
  (trash copper)
  (actions 1)
  (buys 1)
  (coins 0)
  (deck copper copper copper copper estate)
  (hand cellar copper copper estate remodel)
  (plays)
  (discards)))
(act remodel cellar smithy)
#t

;; too-expensive remodel
(move
 ((players p1 p2)
  (supply copper gold mine market)
  (trash copper)
  (actions 1)
  (buys 1)
  (coins 0)
  (deck copper copper copper copper estate)
  (hand cellar copper copper estate remodel)
  (plays)
  (discards)))
(act remodel cellar market)
#f

;; ok smithy
(move
 ((players p1 p2)
  (supply copper gold mine market)
  (trash copper)
  (actions 1)
  (buys 1)
  (coins 0)
  (deck copper copper copper copper estate)
  (hand cellar copper copper estate smithy)
  (plays)
  (discards)))
(act smithy)
#t

;; ok village
(move
 ((players p1 p2)
  (supply copper gold mine market)
  (trash copper)
  (actions 1)
  (buys 1)
  (coins 0)
  (deck copper copper copper copper estate)
  (hand cellar village copper estate smithy)
  (plays)
  (discards)))
(act village)
#t

;; ok woodcutter
(move
 ((players p1 p2)
  (supply copper gold mine market)
  (trash copper)
  (actions 1)
  (buys 1)
  (coins 0)
  (deck copper copper copper copper estate)
  (hand cellar woodcutter copper estate smithy)
  (plays)
  (discards)))
(act woodcutter)
#t

;; ok workshop
(move
 ((players p1 p2)
  (supply copper gold mine village market)
  (trash copper)
  (actions 1)
  (buys 1)
  (coins 0)
  (deck copper copper copper copper estate)
  (hand cellar workshop copper estate smithy)
  (plays)
  (discards)))
(act workshop village)
#t

;; too-expensive workshop
(move
 ((players p1 p2)
  (supply copper gold mine village market)
  (trash copper)
  (actions 1)
  (buys 1)
  (coins 0)
  (deck copper copper copper copper estate)
  (hand cellar workshop copper estate smithy)
  (plays)
  (discards)))
(act workshop market)
#f
