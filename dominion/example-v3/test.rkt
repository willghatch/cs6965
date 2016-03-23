#lang racket
(provide test
         test/values
         test/exn)

(define-syntax-rule (test e expect)
  (check e expect 'e))

(define-syntax-rule (test/values e expect ...)
  (check (call-with-values (lambda () e) list) (list expect ...) 'e))

(define (pretty-format v)
  (define s (open-output-string))
  (pretty-print v s)
  (get-output-string s))

(define (check v expect where)
  (unless (equal? v expect)
    (error 'test "failed: ~.s;\n~aVS.\n~a" 
           where 
           (pretty-format v) 
           (pretty-format expect))))

(define-syntax-rule (test/exn e rx)
  (check-exn (lambda () e) rx 'e))

(define (check-exn thunk rx where)
  (with-handlers ([exn? (lambda (x) 
                          (unless (regexp-match? rx (exn-message x))
                            (error 'test "failed: ~.s; bad message ~e" where (exn-message x))))]
                  [void (lambda (x)
                          (error 'test "failed: ~.s; bad exception ~e" where x))])
    (thunk)
    (error 'test "failed: ~.s; did not raise an exception" where)))
