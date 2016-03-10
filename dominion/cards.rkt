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
(defcard curse
  #:cost 0
  #:victory-points -1)

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

(defcard cellar
  #:cost 2
  #:action
  (λ (state . cards)
    (error "not yet implemented"))
  #:action-validator
  (λ (state act-form)
    #f)
  )
(defcard market
  #:cost 5
  #:action
  (λ (state)
    (error "not yet implemented"))
  #:action-validator
  (λ (state act-form)
    #f)
  )
(defcard remodel
  #:cost 4
  #:action
  (λ (state from to)
    (error "not yet implemented"))
  #:action-validator
  (λ (state act-form)
    #f)
  )
(defcard smithy
  #:cost 4
  #:action
  (λ (state)
    (error "not yet implemented"))
  #:action-validator
  (λ (state act-form)
    #f)
  )
(defcard village
  #:cost 3
  #:action
  (λ (state)
    (error "not yet implemented"))
  #:action-validator
  (λ (state act-form)
    #f)
  )
(defcard woodcutter
  #:cost 3
  #:action
  (λ (state)
    (error "not yet implemented"))
  #:action-validator
  (λ (state act-form)
    #f)
  )
(defcard workshop
  #:cost 3
  #:action
  (λ (state gained)
    (error "not yet implemented"))
  #:action-validator
  (λ (state act-form)
    #f)
  )
(defcard militia
  #:cost 4
  #:action
  (λ (state)
    (error "not yet implemented"))
  #:action-validator
  (λ (state act-form)
    #f)
  )
(defcard witch
  #:cost 5
  #:action
  (λ (state)
    (error "not yet implemented"))
  #:action-validator
  (λ (state act-form)
    #f)
  )
(defcard moat
  #:cost 2
  #:action
  (λ (state)
    (error "not yet implemented"))
  #:action-validator
  (λ (state act-form)
    #f)
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
   'cellar cellar
   'market market
   'remodel remodel
   'smithy smithy
   'village village
   'woodcutter woodcutter
   'workshop workshop
   ))

(define (card-ref name)
  (hash-ref card-hash name))

