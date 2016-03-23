#! /home/mflatt/proj/plt/bin/racket
#lang racket
(require "test.rkt"
         "cards.rkt")

(provide get-hand
         get-supply
         get-num

         try-add
         clean
         pick-discards

         run-player)

(define (get-hand state)
  (cdr (assoc 'hand state)))

(define (get-supply state)
  (cdr (assoc 'supply state)))

(define (get-num key state)
  (cadr (assoc key state)))

;; ----------------------------------------

(define (try-add state)
  (define hand (get-hand state))
  (define treasures (filter treasure? hand))
  (and (pair? treasures)
       `(add ,(car treasures))))

(test (try-add '((hand copper))) '(add copper))
(test (try-add '((hand mine silver))) '(add silver))
(test (try-add '((hand mine))) #f)
  
;; ----------------------------------------

(define (clean state)
  (define hand (get-hand state))
  (if (empty? hand)
      '(clean)
      `(clean ,(car hand))))

(test (clean '((hand copper))) '(clean copper))
(test (clean '((hand))) '(clean))

;; ----------------------------------------

(define (pick-discards state)
  (define hand (get-hand state))
  (define count (max 0 (- (length hand) 3)))
  (define-values (new-hand discards)
    (for/fold ([hand hand] [discards '()]) ([i (in-range count)])
      (define card (or (ormap (lambda (c) (and (member c hand) c))
                              '(estate duchy province
                                       copper silver gold))
                       (car hand)))
      (values (remq card hand)
              (cons card discards))))
  discards)

(test (pick-discards '((hand copper copper copper estate duchy)))
      '(duchy estate))
(test (pick-discards '((hand copper estate duchy)))
      '())
(test (pick-discards '((hand copper estate)))
      '())
(test (pick-discards '((hand mine mine mine mine mine)))
      '(mine mine))

;; ----------------------------------------

(define (run-player try-action try-buy)
  (let loop ()
    (match (read)
      [`(move ,state)
       (write (or (try-action state)
                  (try-add state)
                  (try-buy state)
                  (clean state)))
       (flush-output)]
      [`(moved ,who ,what)
       (void)]
      [`(attacked (act militia) ,name ,state)
       (write `(discard ,@(pick-discards state)))
       (flush-output)]
      [`(defended ,who ,defense)
       (void)])
    (loop)))

