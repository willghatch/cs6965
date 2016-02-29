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
