#lang racket
(require racket/gui/base
         racket/runtime-path
         slideshow/pict
         "driver.rkt"
         "proc.rkt"
         "move.rkt"
         "cards.rkt")

(define programs (let ([v (vector->list (current-command-line-arguments))])
                   (if (null? v)
                       '("player-v3-soldier.rkt" "player-v1.rkt" "player-v2-random.rkt")
                       v)))
(define names (generate-names programs))

;; GUI display state
;;   The statefulness og GUIs essentially
;;   forces us to use a top-level variable:
(define current-game #f)
(define current-turn #f)
(define current-count 0)

;; ----------------------------------------
;; Game drawing

;; card-images : hash from card to image 
(define-runtime-path images-dir "images")
(define card-images
  (for/hash ([c '(back
                  copper silver gold
                  estate duchy province
                  mine cellar market remodel smithy 
                  village woodcutter workshop
                  militia moat)])
    (values c (bitmap (build-path images-dir (format "~a.png" c))))))

;; show-cards : num num list-of-card -> pict
;;  Stacks the given cards, with the first on top and the
;;  cards below the top offset by dx and dy each time
(define (show-cards dx dy cards)
  (panorama
   (for/fold ([p (blank)]) ([c (reverse cards)])
     (lt-superimpose (inset p dx dy 0 0)
                     (hash-ref card-images c)))))

;; game-text : string any ... -> pict
;;  Formats text to show on the screen
(define (game-text s . args)
  (text (apply format s args) 'default 32))

;; draw-player : player turn-or-false -> pict
;;  Generates an image for a player's hand, given
;;  the player and maybe a turn record
(define (draw-player p t)
  (vc-append
   10
   (if t
       (game-text "~a actions  ~a buys  ~a coins"
                  (turn-actions t)
                  (turn-buys t)
                  (turn-coins t))
       (blank))
   (vc-append
    5
    (show-cards 80 0 (if t (turn-plays t) null))
    (hc-append
     20
     ((if (null? (player-deck p)) ghost values) (hash-ref card-images 'back))
     (show-cards -30 -30 (player-hand p))
     (show-cards 2 2 (player-discards p))))
   (game-text "~s   ~s pts" 
              (player-name p)
              (for/fold ([pts 0]) ([c (append (player-deck p)
                                              (player-hand p)
                                              (if t (turn-plays t) null)
                                              (player-discards p))])
                (+ pts (points-of c))))))

;; draw-supply : card -> pict
;;  Generates a picture for a stack in the supply for a given card
(define (draw-supply c)
  (define c-img (hash-ref card-images c))
  (define count (for/fold ([n 0]) ([sc (game-supply current-game)]) 
                  (if (eq? c sc)
                      (add1 n)
                      n)))
  (refocus (vc-append 5
                      ((if (zero? count) ghost values) c-img)
                      (game-text "~a: ~a" c count))
           c-img))

;; spin : list num -> list
;;  Rotates a list `n' times
(define (spin l n)
  (cond
    [(zero? n) l]
    [else (spin (append (cdr l) (list (car l))) (sub1 n))]))

;; draw : dc<%> ->
;;  Creates and draws a picture for the current game state
(define (draw dc)
  (define-values (w h) (send dc get-size))
  (define n (length (game-players current-game)))
  (define ti (modulo current-count n))
  (define image-scale (min (/ w 2000) (/ h 1500)))
  (define players-pict
    (apply
     cc-superimpose
     (for/list ([p (spin (game-players current-game) (- n ti))]
                [i (in-naturals)])
       (rotate (cb-superimpose (scale (draw-player p (and (= i ti) current-turn))
                                      image-scale)
                               (blank (min w h)))
               (* pi -1/2 i)))))
  (define sg 100)
  (define supply-pict
    (scale
     (vc-append 80
                (hc-append sg (draw-supply 'estate) (draw-supply 'duchy) (draw-supply 'province))
                (hc-append sg (draw-supply 'copper) (draw-supply 'silver) (draw-supply 'gold))
                (hc-append sg (draw-supply 'mine) (draw-supply 'cellar) (draw-supply 'market) (draw-supply 'remodel))
                (hc-append sg (draw-supply 'smithy) (draw-supply 'village) (draw-supply 'woodcutter) (draw-supply 'workshop))
                (hc-append sg (draw-supply 'militia) (draw-supply 'moat)))
     (* 0.9 image-scale)))
  (define game-pict (hc-append 30 players-pict supply-pict))
  (draw-pict (cc-superimpose game-pict (blank w h)) dc 0 0))

;; ----------------------------------------
;; GUI set up

(define f
  (new (class frame%
         (define st "")
         (define/override (set-status-text s)
           (set! st s)
           (super set-status-text s))
         (define/public (get-status-text) st)
         (super-new))
       [label "Dominion"]
       [width 900]
       [height 650]))

(define canvas
  (new canvas% 
       [parent f]
       [paint-callback (lambda (c dc)
                         (when current-game
                           (draw dc)))]))

(define hp (new horizontal-panel%
                [parent f]
                [alignment '(center center)]
                [stretchable-height #f]))

(define button
  (new button%
       [parent hp]
       [label "Next"]
       [callback (lambda (b e) (continue))]))
(send button enable #f)

(define auto
  (new check-box%
       [parent hp]
       [label "Auto Next"]
       [callback (lambda (b e)
                   (when (send button is-enabled?)
                     (continue)))]))

(send f create-status-line)

(send f show #t)

(define continue-sema (make-semaphore))

(define (continue)
  (send button enable #f) 
  (semaphore-post continue-sema))

;; --------------------------------------------------
;; Run driver in a separate thread

;; The driver communicates with the GUI by queuing callbacks
;; and waiting on a semaphore before continuing play

(thread
 (lambda ()
   (drive names
          (map start-program programs)
          message-to
          message-from
          (lambda (g t c)
            (queue-callback (lambda ()
                              (set! current-game g)
                              (set! current-turn t)
                              (set! current-count c)
                              (send canvas refresh)
                              (if (send auto get-value)
                                  (semaphore-post continue-sema)
                                  (send button enable #t)))
                            #f)
            (semaphore-wait continue-sema))
          (lambda (m c)
            (queue-callback (lambda () 
                              (send f set-status-text (format "~s" m)))))
          (lambda (d)
            (queue-callback (lambda () 
                              (send f set-status-text (format "~a; ~s" (send f get-status-text) d))))))
   (queue-callback (lambda () 
                     (send f set-status-text "Game Over")))))
