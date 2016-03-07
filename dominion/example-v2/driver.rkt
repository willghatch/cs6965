#lang racket
(require "move.rkt")

(provide drive)

;; drive : list-of-sym list-of-proc 
;;         (proc sexp ->) (proc -> s-exp)
;;         (game turn int ->) (move int ->)
(define (drive names procs message-to message-from turn-hook play-hook)
  ;; Create a initial game state based on the names and number of players:
  (define n (length names))
  (define n-victory (if (= 2 n) 8 12))
  (define init-game
    (game (for/list ([name names])
            (draw-cards
             (player name
                     (shuffle '(estate estate estate 
                                       copper copper copper copper copper copper copper))
                     '()
                     '())))
          (append
           (make-list (- 60 (* 7 n)) 'copper)
           (make-list 40 'silver)
           (make-list 30 'gold)
           (make-list n-victory 'estate)
           (make-list n-victory 'duchy)
           (make-list n-victory 'province)
           (for*/list ([c '(mine cellar market remodel smithy village woodcutter workshop)]
                       [i 10])
             c))
          '()))
  
  ;; take-turn : game turn proc list-of-proc -> game
  ;;  Take a single player's turn, which involves moves until
  ;;  a `clean' message:
  (define (take-turn g t proc other-procs counter)
    (turn-hook g t counter)
    (message-to proc `(move ,(game+turn->state g t)))
    (define play (message-from proc))
    (play-hook play counter)
    (define-values (new-g new-t) (move g t play))
    (for ([other-proc other-procs])
      (message-to other-proc `(moved ,(player-name (car (game-players g)))
                                     ,play)))
    (if new-t
        ;; continue:
        (take-turn new-g new-t proc other-procs counter)
        ;; turn complete:
        new-g))
  
  ;; play : game list-of-proc -> list-of-player
  ;;  Play the game. The list of procs must be kept in sync
  ;;  with the list of players, each rotated after a turn.
  ;;  The result is the list of players at the end of the
  ;;  game.
  (define (play g procs counter)
    (define new-g (take-turn g (turn '() 1 1 0) (car procs) (cdr procs) counter))
    (define new-procs (append (cdr procs) (list (car procs))))
    (if (member 'province (game-supply new-g))
        ;; continue:
        (play new-g new-procs (add1 counter))
        ;; game over:
        (game-players new-g)))
  
  ;; play the game:
  (play init-game procs 0))
