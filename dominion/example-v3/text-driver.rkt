#lang racket
(require "driver.rkt"
         "proc.rkt"
         "move.rkt"
         "cards.rkt")

(define programs (vector->list (current-command-line-arguments)))
(define names (generate-names programs))  

;; report-results : list-of-player -> 
(define (report-results players)
  (define hands (for/list ([p players])
                  (append (player-deck p)
                          (player-hand p)
                          (player-discards p))))
  (define scores (for/list ([hand hands])
                   (for/fold ([v 0]) ([c hand])
                     (+ v (points-of c)))))
  (define winner-score (apply max scores))
  (for ([p players]
        [hand hands]
        [score scores])
    (printf "~a: ~a~a\n" 
            (player-name p)
            score
            (if (= score winner-score) " [WINNER]" ""))
    (printf "  ~s\n" hand)))

(report-results
 (drive names
        (map start-program programs)
        message-to
        message-from
        (lambda (g t c)
          (printf "~s\n" `(move ,(game+turn->state g t))))
        (lambda (m c)
          (printf "~s\n" m))
        (lambda (d)
          (printf "~s\n" d))))

