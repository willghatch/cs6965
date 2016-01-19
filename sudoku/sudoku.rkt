#lang racket/base

(require racket/string)
(require racket/set)

(struct sudoku-board
  (M N rows)
  #:transparent)

(define (empty-sudoku-board m n)
  (let* ([mn (* m n)]
         [empty-row (for/hash ([i (in-range mn)])
                      (values i 'blank))]
         [rows (for/hash ([i (in-range mn)])
                 (values i empty-row))])
    (sudoku-board m n rows)))

(define (set-sudoku-cell b x y val)
  (let* ([rows (sudoku-board-rows b)]
         [row (hash-ref rows y)]
         [nrow (hash-set row x val)]
         [nrows (hash-set rows y nrow)])
    (struct-copy sudoku-board b [rows nrows])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string->board s)
  (define lines
    (filter (λ (l) (not (null? l)))
            (for/list ([line (string-split s "\n")])
              (string-split line))))
  ;; TODO - handle errors
  (define m (string->number (caar lines)))
  (define n (string->number (cadar lines)))
  (define data (list->vector (map list->vector (cdr lines))))
  (for*/fold ([board (empty-sudoku-board m n)])
             ([x (in-range (* m n))]
              [y (in-range (* m n))])
    (let ((v (vector-ref (vector-ref data y) x)))
      (if (equal? v "_")
          board
          (set-sudoku-cell board x y (string->number v))))))

(module+ test
  (require rackunit)
  (check-equal? (string->board "2 1\n1 _\n2 _")
                (set-sudoku-cell (set-sudoku-cell (empty-sudoku-board 2 1) 0 1 2) 0 0 1)))

(define (board->string b #:extra-space? [extra-space? #f])
  (let* ([m (sudoku-board-M b)]
         [n (sudoku-board-N b)]
         [mn (* m n)])
    (string-append
     (format "~a ~a~n" m n)
     (if extra-space? "\n" "")
     (apply
      string-append
      (for*/list ([y (in-range mn)]
                  [x (in-range mn)])
        (string-append
         (let ([cell (get-sudoku-cell b x y)])
           (if (number? cell)
               (number->string cell)
               "_"))
         (if (not (equal? x (sub1 mn))) " " "")
         ;; print an extra space between groups
         (if (and extra-space?
                  (equal? (sub1 m) (modulo x m))
                  (not (equal? x (sub1 mn))))
             " "
             "")
         (if (and (equal? x (sub1 mn)) (not (equal? y (sub1 mn))))
             ;; extra vertical space between groups
             (if (and extra-space? (equal? (sub1 n) (modulo y n)))
                 "\n\n"
                 "\n")
             "")
         )))
     "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; projectors

(define (get-sudoku-cell board x y)
  (hash-ref (hash-ref (sudoku-board-rows board) y) x))

(define (get-peer-group-cells board x y)
  ;; m is the number of columns in a sub-board
  ;; n is the number of rows in a sub-board
  (let* ([m (sudoku-board-M board)]
         [n (sudoku-board-N board)]
         ;; start of peer group ranges
         [px (* m (quotient x m))]
         [py (* n (quotient y n))])
    (for*/list ([cx (in-range px (+ px m))]
                [cy (in-range py (+ py n))])
      (get-sudoku-cell board cx cy))))

(define (get-row-cells board y)
  (hash-values (hash-ref (sudoku-board-rows board) y)))

(define (get-column-cells board x)
  (for/list ([y (in-range 0 (* (sudoku-board-N board)
                               (sudoku-board-M board)))])
    (get-sudoku-cell board x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; validators

(define (valid-group? g)
  ;; a group will be a list of m*n values
  (foldl (λ (v s) (cond [(not s) #f]
                        [(equal? v 'blank) s]
                        [(set-member? s v) #f]
                        [else (set-add s v)]))
         (set)
         g))
(module+ test
  (check-not-false (valid-group? '(1  3  5)))
  (check-false (valid-group? '(1 5 5))))

(define (valid-board-row? b y)
  (valid-group? (get-row-cells b y)))
(define (valid-board-column? b x)
  (valid-group? (get-column-cells b x)))
(define (valid-peer-group? b x y)
  (valid-group? (get-peer-group-cells b x y)))

(define (valid-board? b)
  (let* ([m (sudoku-board-M b)]
         [n (sudoku-board-N b)]
         [mn (* m n)])
    (and
     ;; rows have unique cells
     (for/and ([i (in-range mn)])
       (valid-board-row? b i))
     ;; columns have unique cells
     (for/and ([i (in-range mn)])
       (valid-board-column? b i))
     ;; sub-boards have unique cells
     ;; m is the number of columns in a sub-board
     ;; n is the number of rows in a sub-board
     (for/and ([i m])
       (for/and ([j n])
         (valid-peer-group? b (* j m) (* i n)))))))


(module+ test
  (define bad-board-string #<<EOB
3 3

_ _ _  _ _ _  1 _ _
_ _ _  1 1 _  3 _ _
_ _ _  _ 1 _  2 _ _

_ _ _  _ _ _  5 _ _
_ _ _  _ _ _  6 _ _
1 2 3  4 5 6  7 8 9

_ _ _  _ _ _  4 _ _
_ _ _  _ _ _  8 _ _
_ _ _  _ _ _  9 _ _

EOB
)
  (define good-board-string #<<EOB
3 3

_ _ _  _ _ _  1 _ _
_ _ _  _ _ _  3 _ _
_ _ _  _ _ _  2 _ _

_ _ _  _ _ _  5 _ _
_ _ _  _ _ _  6 _ _
1 2 3  4 5 6  7 8 9

_ _ _  _ _ _  4 _ _
_ _ _  _ _ _  8 _ _
_ _ _  _ _ _  9 _ _

EOB
)
  (define bad-board (string->board bad-board-string))
  (define good-board (string->board good-board-string))
  (check-equal? bad-board-string (board->string bad-board #:extra-space? #t))
  (check-false (valid-board-row? bad-board 1))
  (check-false (valid-board-column? bad-board 4))
  (check-false (valid-peer-group? bad-board 3 0))
  (check-not-false (valid-board-row? bad-board 5))
  (check-not-false (valid-board-column? bad-board 6))
  (check-not-false (valid-peer-group? bad-board 6 5))
  (check-false (valid-board? bad-board))
  (check-not-false (valid-board? good-board))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-board-potential-values b)
  (list->set (in-range 1 (add1 (* (sudoku-board-M b) (sudoku-board-N b))))))

(define (get-cell-potential-values b x y)
  (let ([rcs (get-row-cells b y)]
        [ccs (get-column-cells b x)]
        [pcs (get-peer-group-cells b x y)])
    (set-subtract (get-board-potential-values b)
                  (list->set rcs)
                  (list->set ccs)
                  (list->set pcs))))

(module+ test
  (check-equal? (get-cell-potential-values good-board 8 3)
                (set 1 2 3 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (solve-rec b x y n-max solutions-so-far)
  (let* ([m (sudoku-board-M b)]
         [n (sudoku-board-N b)]
         [mn (* m n)]
         [cell (if (and (< x mn) (< y mn))
                   (get-sudoku-cell b x y)
                   'out-of-bounds)])
    (cond [(<= n-max (set-count solutions-so-far)) solutions-so-far]
          [(>= x mn) (solve-rec b 0 (add1 y) n-max solutions-so-far)]
          [(>= y mn) (set-add solutions-so-far b)]
          [(number? cell) (solve-rec b (add1 x) y n-max solutions-so-far)]
          [(equal? cell 'blank)
           (for/fold ([solutions solutions-so-far])
                     ([p (get-cell-potential-values b x y)])
             (solve-rec (set-sudoku-cell b x y p) (add1 x) y n-max solutions))
           ])))
(define (solve board n-solutions)
  ;; returns a set of solutions
  (if (valid-board? board)
      (solve-rec board 0 0 n-solutions (set))
      (set)))
(define (solve-unique board)
  (let ((s (solve board 1)))
    (cond [(set-empty? s) #f]
          [(> (set-count s) 1) 'multiple]
          [else (set-first s)])))

(module+ test
  (check-false (solve-unique bad-board))

  (define test-1-string #<<EOB
3 3
_ 5 _ _ 6 _ _ _ 1
_ _ 4 8 _ _ _ 7 _
8 _ _ _ _ _ _ 5 2
2 _ _ _ 5 7 _ 3 _
_ _ _ _ _ _ _ _ _
_ 3 _ 6 9 _ _ _ 5
7 9 _ _ _ _ _ _ 8
_ 1 _ _ _ 6 5 _ _
5 _ _ _ 3 _ _ 6 _

EOB
    )
  (define test-1-solution-string #<<EOB
3 3
9 5 3 7 6 2 8 4 1
6 2 4 8 1 5 9 7 3
8 7 1 3 4 9 6 5 2
2 8 9 4 5 7 1 3 6
1 6 5 2 8 3 4 9 7
4 3 7 6 9 1 2 8 5
7 9 6 5 2 4 3 1 8
3 1 8 9 7 6 5 2 4
5 4 2 1 3 8 7 6 9

EOB
    )
  (define test-1 (string->board test-1-string))
  (check-equal? (solve-unique test-1) (string->board test-1-solution-string))

  (check-equal? (set-count (solve (string->board "1 2 \n _ _ \n _ _") +inf.f))
                2)


  )
