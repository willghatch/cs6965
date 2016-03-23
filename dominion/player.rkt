#lang racket/base

(require "lib.rkt")
(require "card.rkt")
(require "cards.rkt")
(require "gamestate.rkt")
(require racket/list)
(require racket/match)
(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

(define (play-loop)
  (let ([message (read)])
    (match message
      [(list moved name play) (void)]
      [(list attacked attack-form name state)
       (do-defend (parse-state state) attack-form)]
      [(list move state)
       (do-play (parse-state state))]))
  (play-loop))

(define (do-play state)
  (printf "~a~n" (decide-play state))
  (flush-output))

;; non-hygienically introduce identifiers for the parts of state
(define-syntax (define/state stx)
  (syntax-case stx ()
    [(d (name state arg ...) body ...)
     #`(define (name state arg ...)
         (let ([#,(datum->syntax #'d 'players) (gamestate-players state)]
               [#,(datum->syntax #'d 'supply) (gamestate-supply state)]
               [#,(datum->syntax #'d 'actions) (gamestate-actions state)]
               [#,(datum->syntax #'d 'buys) (gamestate-buys state)]
               [#,(datum->syntax #'d 'coins) (gamestate-coins state)]
               [#,(datum->syntax #'d 'deck) (gamestate-deck state)]
               [#,(datum->syntax #'d 'hand) (gamestate-hand state)])
           body ...))]
    [else (raise-syntax-error #f "define/state - bad syntax" stx)]))

(define/state (decide-play state)
  (or
   (and {actions . > . 0} (decide-action state))
   (and (member gold hand) '(add gold))
   (and (member silver hand) '(add silver))
   (and (member copper hand) '(add copper))
   (and {buys . > . 0} (decide-buy state))
   (and (empty? hand) '(clean))
   `(clean ,(card-name (first hand)))
   ))

(define/state (decide-action state)
  (or
   (and (member village hand)
        '(act village))
   (and (member market hand)
        '(act market))
   (and {actions . > . 1}
        (member smithy hand)
        '(act smithy))
   ;; This complicated handling of cellar might be better, since
   ;; it gets rid of coppers.  Except that it might leave coppers un-minable.
   (let* ([vcs (filter (λ (c) {0 . < . (card-victory-points c)})
                       hand)]
          [coppers (filter (λ (c) (equal? copper c))
                           hand)]
          [deck-has-sg (not (null? (filter (λ (c) (or (equal? gold c)
                                                      (equal? silver c)))
                                           deck)))])
     (and (member cellar hand)
          (if (and (not (null? coppers))
                   deck-has-sg)
              `(act cellar ,@(map card-name vcs) ,@(map card-name coppers))
              (and vcs `(act cellar ,@(map card-name vcs))))))
   (and (member mine hand)
        (member copper hand)
        (member silver supply)
        '(act mine copper silver))
   (and (member mine hand)
        (member silver hand)
        (member gold supply)
        '(act mine silver gold))
   (and (member smithy hand)
        '(act smithy))
   (and (member militia hand)
        '(act militia))
   (and (member witch hand)
        '(act witch))
   (and (member moat hand)
        '(act moat))
   (and (member woodcutter hand)
        '(act woodcutter))
   (and (member workshop hand)
        (blingest/budget state 4)
        `(act workshop ,(card-name (blingest/budget state 4))))
   (and (member remodel hand)
        (not (empty? (remove remodel hand)))
        (let* ([to-trash (first (sort (remove remodel hand) < #:key card-cost))]
               [blingest (blingest/budget state (+ 2 (card-cost to-trash)))])
          (and blingest
               {(card-cost blingest) . > . (card-cost to-trash)}
               `(act remodel ,(card-name to-trash) ,(card-name blingest)))
          ))
   #f
   ))

(define/state (supply-available-for state max-cost)
  (filter (λ (c) (>= max-cost (card-cost c))) supply))
(define/state (supply-available-for/no-crap state max-cost)
  (filter (λ (c) (not (or (equal? c copper)
                          (equal? c curse))))
          (supply-available-for state max-cost)))
(define (expensive-est-sort cards)
  (reverse (sort cards < #:key card-cost #:cache-keys? #t)))
(define/state (blingest/budget state budget)
  (let ([bling (expensive-est-sort
                (shuffle (supply-available-for/no-crap state budget)))])
    (if (empty? bling)
        #f
        (first bling))))

(define/state (decide-buy state)
  (let ([bling (blingest/budget state coins)])
    (if bling
        `(buy ,(card-name (blingest/budget state coins)))
        #f)))


(define (do-defend state attack-form)
  (printf "~a~n" (decide-defend state attack-form))
  (flush-output))
(define/state (decide-defend state attack-form)
  (or
   (and (member moat hand) '(moat))
   (match attack-form
     ['(act militia)
      (decide-discard state (- (length hand) 3))]
     [else #f])))

(define/state (decide-discard state n-discards)
  (define (decide-discard-1 hand-left)
    (or
     (and (member estate hand-left) estate)
     (and (member duchy hand-left) duchy)
     (and (member province hand-left) province)
     (and (member curse hand-left) curse)
     (and (member copper hand-left) copper)
     (first (shuffle hand-left))))
  (define (rec discards hand-left)
    (if (equal? (length discards) n-discards)
        discards
        (let ([new-discard (decide-discard-1 hand-left)])
          (rec (cons new-discard discards)
               (remove new-discard hand-left)))))
  `(discard ,@(map card-name (rec empty hand)))
  )


(module+ main
  (play-loop))

