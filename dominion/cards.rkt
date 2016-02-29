#lang racket/base

(require "card.rkt")
(require "gamestate.rkt")
(require racket/match)
(require threading)
(require lens)

(provide (all-defined-out))

(defcard copper
  #:cost 0
  #:coin-value 1
  #:treasure? #t)
(defcard silver
  #:cost 3
  #:coin-value 2
  #:treasure? #t)
(defcard gold
  #:cost 6
  #:coin-value 3
  #:treasure? #t)

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
  #:action
  (λ (state from to)
    (let ([to (card-ref to)]
          [from (card-ref from)])
      (~> state
          (lens-transform gamestate-trash-lens
                          _
                          (λ (trash) (cons from trash)))
          (lens-transform gamestate-supply-lens
                          _
                          (λ (supply) (remove to supply)))
          (lens-transform gamestate-discards-lens
                          _
                          (λ (discards) (cons to discards)))
          )))
  #:action-validator
  (λ (state act-form)
    (and
     (< 0 (gamestate-actions state))
     (with-handlers ([(λ _ #t) (λ _ #f)])
       (match act-form
         [(list 'act 'mine old-t new-t)
          (let ([old (card-ref old-t)]
                [new (card-ref new-t)])
            (and (card-treasure? old)
                 (card-treasure? new)
                 (member (gamestate-supply new))
                 (<= 3
                     (- (card-cost new)
                        (card-cost old)))))]
         [else #f]))))
  )


(define card-hash
  (hash
   'copper copper
   'silver silver
   'gold gold
   'estate estate
   'duchy duchy
   'province province
   'mine mine
   ))

(define (card-ref name)
  (hash-ref card-hash name))

