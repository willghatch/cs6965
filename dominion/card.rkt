#lang racket/base

(provide (all-defined-out))
(require lens)

(struct/lens card
  (name
   cost
   coin-value
   victory-points
   action
   action-validator
   treasure?)
  #:transparent)

(define-syntax-rule (defcard name arg ...)
  (define name (mk-card (quote name) arg ...)))
(define (mk-card name
                 #:cost [cost 0]
                 #:coin-value [coin-value 0]
                 #:victory-points [victory-points 0]
                 #:action [action #f]
                 #:action-validator [action-validator #f]
                 #:treasure? [treasure? #f])
  (card name
        cost
        coin-value
        victory-points
        action
        action-validator
        treasure?))

