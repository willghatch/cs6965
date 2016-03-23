#lang racket

(provide (all-defined-out))

(define (treasure? t)
  (memq t '(copper silver gold)))

(define (victory? t)
  (memq t '(estate duchy province)))

(define (action? t)
  (memq t '(mine cellar market remodel 
                 smithy village woodcutter workshop
                 militia moat)))

(define (card? t)
  (or (treasure? t)
      (victory? t)
      (action? t)))

(define (treasure-value c)
  (case c
    [(copper) 1]
    [(silver) 2]
    [(gold) 3]))

(define (cost-of c)
  (case c
    [(copper) 0]
    [(silver) 3]
    [(gold) 6]
    [(estate) 2]
    [(duchy) 5]
    [(province) 8]
    [(mine) 5]
    [(cellar) 2]
    [(market) 5]
    [(remodel) 4]
    [(smithy) 4]
    [(village) 3]
    [(woodcutter) 3]
    [(workshop) 3]
    [(militia) 4]
    [(moat) 2]))

(define (points-of c)
  (case c
    [(estate) 1]
    [(duchy) 3]
    [(province) 6]
    [else 0]))
