#lang racket
(provide start-program
         message-to
         message-from
         generate-names)

(struct proc (o i))

(define (start-program p)
  (define-values (s o i e) (subprocess #f #f #f p))
  (thread (lambda () (copy-port e (current-error-port))))
  (proc o i))

(define (message-to proc v)
  (writeln v (proc-i proc))
  (flush-output (proc-i proc)))

(define (message-from proc)
  (read (proc-o proc)))

(define (generate-names progs)
  (for/list ([p progs]
             [i (in-naturals)])
    (define-values (base name dir?) (split-path p))
    (string->symbol (format "~a-~a" i (path-replace-suffix name #"")))))
