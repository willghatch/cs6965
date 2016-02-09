#lang racket/base

(require racket/string)
(require racket/set)
(require racket/math)
(require lens)

(module+ test (require rackunit))

(provide (all-defined-out))

(struct sudoku-board
  (M N rows)
  #:transparent)

(define (empty-sudoku-board m n)
  (let* ([mn (* m n)]
         [empty-row (for/hash ([i (in-range mn)])
                      (values i (get-board-potential-values (sudoku-board m n #f))))]
                      ;(values i 'blank))]
         [rows (for/hash ([i (in-range mn)])
                 (values i empty-row))])
    (sudoku-board m n rows)))

(define board-rows-lens (make-lens sudoku-board-rows
                                   (λ (b r) (struct-copy sudoku-board b [rows r]))))
(define (board-to-cell-lens x y) (hash-ref-nested-lens y x))
(define (cell-lens x y) (lens-compose (board-to-cell-lens x y) board-rows-lens))
(define (get-sudoku-cell board x y)
  (lens-view (cell-lens x y) board))
(define (set-sudoku-cell/no-update board x y v)
  (lens-set (cell-lens x y) board v))

(define (get-m*n board)
  (* (sudoku-board-M board) (sudoku-board-N board)))

(define (get-row-coordinates board _ y)
  (for/list ([x (in-range (get-m*n board))])
    (cons x y)))
(define (get-column-coordinates board x _)
  (for/list ([y (in-range (get-m*n board))])
    (cons x y)))
(define (get-peer-group-coordinates board x y)
  ;; m is the number of columns in a sub-board
  ;; n is the number of rows in a sub-board
  (let* ([m (sudoku-board-M board)]
         [n (sudoku-board-N board)]
         ;; start of peer group ranges
         [px (* m (quotient x m))]
         [py (* n (quotient y n))])
    (for*/list ([cx (in-range px (+ px m))]
                [cy (in-range py (+ py n))])
      (cons cx cy))))

(define (get-cells board coord-func x y)
  (for/list ([coord (coord-func board x y)])
    (get-sudoku-cell board (car coord) (cdr coord))))

(define (get-peer-group-cells board x y)
  (get-cells board get-peer-group-coordinates x y))
(define (get-row-cells board y)
  (get-cells board get-row-coordinates #f y))
(define (get-column-cells board x)
  (get-cells board get-column-coordinates x #f))

(define (update-potential-values board x y)
  (define v (get-sudoku-cell board x y))
  (define (xform setval)
    (if (set? setval)
        (set-remove setval v)
        setval))
  (if (set? v)
      board
      (for/fold ([b board])
                ([c (append (get-row-coordinates board x y)
                            (get-column-coordinates board x y)
                            (get-peer-group-coordinates board x y))])
        (lens-transform (cell-lens (car c) (cdr c)) b xform))))

(define (un-set-single-values board)
  ;; when I have a set of 1 possible value, lift it out of the set and re-update
  (define (un-set-rec board x y)
    (let ([mn (get-m*n board)])
      (cond [(x . >= . mn) (un-set-rec board 0 (add1 y))]
            [(y . >= . mn) board]
            [else
             (let ([v (get-sudoku-cell board x y)])
               (if (and (set? v) (equal? (set-count v) 1))
                   (set-sudoku-cell/update board x y (set-first v))
                   (un-set-rec board (add1 x) y)))])))
  (un-set-rec board 0 0))
(define (post-set-update board x y)
  (un-set-single-values (update-potential-values board x y)))

(define (set-sudoku-cell/update b x y v)
  (post-set-update (set-sudoku-cell/no-update b x y v) x y))
(define set-sudoku-cell set-sudoku-cell/update)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string->board s)
  (define parts (string-split s))
  (define l (length parts))
  (when (< l 3) (error "invalid board"))
  (let ([m (string->number (car parts))]
        [n (string->number (cadr parts))]
        [elems (list->vector (cddr parts))])
    (when (< (vector-length elems) (* m n)) (error "invalid board"))
    (for*/fold ([board (empty-sudoku-board m n)])
               ([x (in-range (* m n))]
                [y (in-range (* m n))])
      (let ([v (vector-ref elems (+ x (* m n y)))])
        (if (or (equal? v "_") (equal? v "0"))
            board
            (set-sudoku-cell board x y (string->number v)))))))

(module+ test
  (check-equal? (string->board "2 1\n1 _\n2 _")
                (set-sudoku-cell (set-sudoku-cell (empty-sudoku-board 2 1) 0 1 2) 0 0 1)))

(define (board->string b #:extra-space? [extra-space? #t])
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; validators

(define (valid-group? g)
  ;; a group will be a list of m*n values
  (foldl (λ (v s) (cond [(not s) #f]
                        [(set? v) s]
                        ;[(equal? v 'blank) s]
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
  (let ([v (get-sudoku-cell b x y)])
    (if (set? v) v (set v))))
(define (get-cell-potential-values/old b x y)
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
(define (solve-rec/sort b n-max solutions-so-far)
  (if (>= (set-count solutions-so-far) n-max)
      solutions-so-far
      (let* ([m (sudoku-board-M b)]
             [n (sudoku-board-N b)]
             [mn (* m n)]
             [coords (for*/list ([x (in-range mn)]
                                 [y (in-range mn)])
                       (cons x y))])
        (let-values ([(mincoord npossible)
                      (for/fold ([mincoord (cons 0 0)]
                                 [minv +inf.0])
                                ([c coords])
                        (let ([v (if (number? (get-sudoku-cell b (car c) (cdr c)))
                                     +inf.0
                                     (set-count (get-cell-potential-values b (car c) (cdr c))))])
                          (if (> minv v)
                              (values c v)
                              (values mincoord minv))))])
          (if (infinite? npossible)
              (set-add solutions-so-far b)
              (for/fold ([solutions solutions-so-far])
                        ([p (get-cell-potential-values b (car mincoord) (cdr mincoord))])
                (solve-rec/sort (set-sudoku-cell b (car mincoord) (cdr mincoord) p)
                                n-max
                                solutions)))
          ))))

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
          ;[(equal? cell 'blank)
          [(set? cell)
           (for/fold ([solutions solutions-so-far])
                     ([p (get-cell-potential-values b x y)])
             (solve-rec (set-sudoku-cell b x y p) (add1 x) y n-max solutions))
           ])))
(define (solve board n-solutions)
  ;; returns a set of solutions
  (if (valid-board? board)
      (solve-rec/sort board n-solutions (set))
      (set)))
(define (solve-unique board)
  (let ((s (solve board 2)))
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


(define (generate-board m n [fuel 500])
  (define mn (* m n))
  (define (rec b b/no-update fuel)
    (if (< fuel 0)
        #f
        (let* ([x (random mn)]
               [y (random mn)]
               [v-possible (get-cell-potential-values b x y)]
               [v (if (set-empty? v-possible)
                      ;; if nothing is possible, just choose 1 and it will later show its invalid, and use a fuel
                      1
                      (list-ref (set->list v-possible) (random (set-count v-possible))))]
               [nb/no-update (set-sudoku-cell/no-update b/no-update x y v)]
               [nb (set-sudoku-cell b x y v)]
               [valid? (valid-board? nb)]
               [solution (and valid? (solve-unique nb))])
          (cond
            ;; if there is a unique solution, use the current board
            [(sudoku-board? solution) nb/no-update]
            ;; if there is no solution, try going forward with the last good board
            [(not solution) (rec b b/no-update (- fuel 1))]
            ;; there are multiple solutions still, so let's prune some more
            [else (rec nb nb/no-update fuel)]))))

  (or (let ([b (empty-sudoku-board m n)])
        (rec b b fuel))
      (generate-board m n fuel)))

(module+ main
  (let ([b (generate-board 3 3)])
    (printf "~a~n" (board->string b))
    (printf "~a~n" (board->string (solve-unique (string->board (board->string b)))))
    ))
