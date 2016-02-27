#lang racket/base

(require "card.rkt")


(defcard copper
  #:cost 0
  #:coin-value 1
  #:treasure-card? #t)
(defcard silver
  #:cost 3
  #:coin-value 2
  #:treasure-card? #t)
(defcard gold
  #:cost 6
  #:coin-value 3
  #:treasure-card? #t)

(defcard estate
  #:cost 2
  #:victory-points 1)
(defcard duchy
  #:cost 5
  #:victory-points 3)
(defcard province
  #:cost 8
  #:victory-points 6)

(defcard mine
  #:cost 5
  #:action (λ (playstate from to)
             ;; TODO - put an appropriate action here
             playstate))


(println mine)
