#lang racket
(require "driver.rkt"
         "proc.rkt"
         "move.rkt"
         "cards.rkt")

(define all-programs (vector->list (current-command-line-arguments)))
(define all-names (for/list ([prog (in-list all-programs)])
                    (define-values (base name dir?) (split-path prog))
                    (define-values (d-base d-name d-dir?) (split-path base))
                    (path-element->string d-name)))

(define (random-four len)
  (let loop ([l null])
    (cond
     [(= 4 (length l))
      l]
     [else
      (define n (random len))
      (if (member n l)
          (loop l)
          (loop (cons n l)))])))

(define current-player (make-parameter #f))
(define current-message (make-parameter #f))

(define (show-status plays wins fails)
  (printf "\nName         Games      Wins       Fails\n") 
  (define (pad v n) (substring (format "~a~a" v (make-string n #\space))
                               0
                               n))
  (define (zero->blank v) (if (zero? v) "" v))
  (for ([n (in-list all-names)])
    (printf " ~a : ~a ~a ~a\n" 
            (pad (format "~s" n) 10) 
            (pad (hash-ref plays n 0) 10)
            (pad (zero->blank (hash-ref wins n 0)) 10)
            (pad (zero->blank (hash-ref fails n 0)) 10))))

(define all-candidates (map cons all-names all-programs))

(let loop ([candidates all-candidates] [plays (hash)] [wins (hash)] [fails (hash)])
  (define ns (random-four (length candidates)))
  (define-values (names programs)
    (for/lists (names programs) ([n (in-list ns)]) 
               (define c (list-ref candidates n))
               (values (car c) (cdr c))))
  (printf "\nplaying ~s\n" names)
  (define cust (make-custodian))
  (define procs (parameterize ([current-custodian cust])
                  (map start-program programs)))
  (define proc-to-name (for/hash ([name (in-list names)]
                                  [proc (in-list procs)])
                         (values proc name)))
  (define-values (result-players failed)
    (with-handlers ([exn? (lambda (exn)
                            (define name (current-player))
                            (define msg (current-message))
                            ((error-display-handler) (exn-message exn) exn)
                            (printf ">> failure blamed on ~s <<\n" name)
                            (values null name))])
      (values
       (drive names
              procs
              (lambda (proc s-exp) 
                (current-message s-exp)
                (current-player (hash-ref proc-to-name proc))
                (message-to proc s-exp))
              (lambda (proc)
               (current-player (hash-ref proc-to-name proc))
                (message-from proc))
              (lambda (g t c) 
                (current-message #f)
                (current-player (player-name (car (game-players g)))))
              (lambda (m c) (void))
              (lambda (d) (void)))
       #f)))
  (custodian-shutdown-all cust)
  (define scores
    (for/list ([p (in-list result-players)])
      (define name (player-name p))
      (define score
        (for/fold ([pts 0]) ([c (append (player-deck p)
                                        (player-hand p)
                                        (player-discards p))])
          (+ pts (points-of c))))
      (printf " ~s ~s\n" name score)
      (cons name score)))
  (define max-score (apply max 0 (map cdr scores)))
  (define new-plays
    (for/fold ([plays plays]) ([s (in-list scores)])
      (hash-update plays (car s) add1 0)))
  (define new-wins
    (for/fold ([wins wins]) ([s (in-list scores)])
      (if (= (cdr s) max-score)
          (hash-update wins (car s) add1 0)
          wins)))
  (define new-fails
    (if failed
        (hash-update fails failed add1 0)
        fails))
  (define next-candidates
    (for/list ([c (in-list candidates)]
               #:unless (member (car c) names))
      c))
  (show-status new-plays new-wins new-fails)
  (loop (if ((length next-candidates) . >= . 4)
            next-candidates
            all-candidates)
        new-plays new-wins new-fails))
