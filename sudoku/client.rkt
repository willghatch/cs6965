#lang racket/base

(require net/http-client)
(require racket/port)
(require "sudoku.rkt")

(define (get-puzzle host m n)
  (define-values (status headers content-port)
    (http-sendrecv host
                   (string-append (number->string m)
                                  "x"
                                  (number->string n)
                                  ".txt")
                   #:port 8080
                   #:method "get"
                   ))
  (port->string content-port))


(module+ main
  (printf "Requesting puzzle.~n")
  (define p (get-puzzle "localhost" 2 3))
  (printf "got puzzle:~n~a~n" p)
  (printf "solution:~n~a~n" (board->string (solve-unique (string->board p))))
  )
