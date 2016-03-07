#lang racket
(require "move.rkt")

;; notification->game+turn : notification -> (values game turn)
;;  Synthesizes game and turn structs given an input state.
;;  The synthesized game has only one player, but that's
;;  good enough for testing the expected inputs.
(define (notification->game+turn state)
  (match state
    [`(move
       ((players ,names ...)
        (supply ,supply ...)
        (trash ,trash ...)
        (actions ,actions)
        (buys ,buys)
        (coins ,coins)
        (deck ,deck ...)
        (hand ,hand ...)
        (plays ,plays ...)
        (discards ,discards ...)))
     (values (game (list
                    (player (car names)
                            deck
                            hand
                            discards))
                   supply
                   trash)
             (turn plays actions buys coins))]
    [else
     (error 'test "bad notification: ~e" state)]))

;; Loop while inputs are available:
(let loop ()
  (define state (read))
  (unless (eof-object? state)
    (define plays+ok? 
      ;; Read a boolean-terminated list of plays:
      (let loop ()
        (define v (read))
        (if (boolean? v)
            (list v)
            (cons v (loop)))))
    ;; Drop the boolean for `plays':
    (define plays (take plays+ok? (sub1 (length plays+ok?))))
    ;; The ending boolean is `ok?':
    (define ok? (last plays+ok?))

    (define-values (g t) (notification->game+turn state))
    (define failed (with-handlers ([exn:fail? (lambda (exn) (exn-message exn))])
                     ;; try all plays:
                     (for/fold ([g g] [t t]) ([play plays])
                       (move g t play))
                     #f))

    (unless (equal? ok? (not failed))
      (error 'test "failed: ~s ~s => ~s\n"
             state plays (or failed "[allowed]")))
    ;(printf "Ok: ~a\n" (or failed "[allowed]"))

    (loop)))
