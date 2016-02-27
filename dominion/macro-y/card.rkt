#lang racket/base

(provide 
         defcard
         )

(require (for-syntax
          syntax/parse
          racket/stxparam
          racket/base
          ))


(begin-for-syntax
  (define card-parts '(name cost coin-value victory-points action treasure-card?))

  (define keyword->symbol (compose string->symbol keyword->string))
  (define symbol->keyword (compose string->keyword symbol->string))
  (define card-parts-kw (map symbol->keyword card-parts))

  (define-splicing-syntax-class kv-pair
    (pattern (~seq name:keyword value)))

  (define card-attr-defaults
    (hash 'name 'blank
          'cost 0
          'coin-value 0
          'victory-points 0
          'action #f
          'treasure-card? #f))


  )

(define-syntax-parameter card-id #'card)

(define-syntax (defcardstruct stx)
  #`(struct (card-id) #,card-parts #:transparent))
(defcardstruct)

(define-syntax (defcard stx)
  (syntax-parse stx
    [(defcard name:id attr:kv-pair ...)
     (let* (
            [attr-hash (for/hash ([n (map keyword->symbol (syntax->datum #'(attr.name ...)))]
                                  [v (syntax-e #'(attr.value ...))])
                         (values n v))]

            [full-attr-hash (for/hash ([n card-parts])
                              (values n (if (hash-has-key? attr-hash n)
                                            (hash-ref attr-hash n)
                                            (hash-ref card-attr-defaults n))))])
       #`(define name ((card-id) 'name #,@(map (λ (n) (hash-ref full-attr-hash n))
                                          (cdr card-parts))))
       )]))


#;(struct card
  (name
   cost
   coin-value
   victory-points
   action
   treasure-card?)
  #:transparent)

