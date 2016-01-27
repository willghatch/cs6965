#lang racket/base
(require web-server/servlet)
(require web-server/servlet-env)
(require racket/string)
(require racket/list)
(require "sudoku.rkt")

(define (serve-sudoku request)
  (let* ([url (url->string (request-uri request))]
         [mn-match (regexp-match #px"\\d+x\\d+" url)]
         [first-match (if (and mn-match (not (null? mn-match)))
                          (first mn-match)
                          "2x3")]
         [match-split (string-split first-match "x")]
         [m (string->number (first match-split))]
         [n (string->number (second match-split))]
         [b (generate-board m n)])
    (printf "request path: ~a~n" url)
    (printf "serving:~n")
    (printf "~a~n" (board->string b))
    (printf "~a~n" (board->string (solve-unique b)))
    (response/full 200 #"OK" (current-seconds) #"text/plain" null (list (string->bytes/utf-8 (board->string b))))

    )
  )

;(no-web-browser)
(serve/servlet serve-sudoku
               #:servlet-regexp #rx""
               )
