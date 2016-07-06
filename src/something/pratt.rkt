#lang racket/base
;; Pratt parsing, with a slight extension for adjacency as a pseudo-operator.

(provide raw-pratt-parse
         pratt-parse
         ensure-no-leftover-tokens
         (struct-out operator))

(require racket/match)
(require (only-in racket/list splitf-at))

(struct operator (token binding-power associativity handler) #:transparent)

;; An Input is a (Listof Token).

;; A Parser is a (Input ;; inputs
;;                Natural ;; "right binding power"
;;                (Value (Listof Token) -> ParseResult) ;; continuation
;;                -> ParseResult)

;; A ParseResult is a (Values Value ;; semantic value
;;                            Input) ;; remaining unconsumed inputs

;; An Operator is one of
;;  - (operator Token Natural 'left (Token Value Value -> Value))
;;  - (operator Token Natural 'nonassoc (Token Value Value -> Value))
;;  - (operator Token Natural 'n-ary (Token Value Value -> Value))
;;  - (operator Token Natural 'right (Token Value Value -> Value))
;;  - (operator Token Natural 'prefix (Token Value -> Value))
;;  - (operator Token Natural 'prefix-macro (Token Parser Input -> ParseResult))
;;  - (operator Token Natural 'postfix (Token Value -> Value))

;; An OperatorTable is a (Token -> (Option Operator)).

;; (Non-assocative and n-ary operators are treated almost identically
;; to left-associative operators, and so must be explicitly handled in
;; the semantic-function contained in each table rule. Adjacency in
;; the `test` module shows an example: n-ary function call is exactly
;; a non-associative operator.)

;; Input
;; (Parser Input -> ParseResult)
;; (Token -> Natural)
;; (Token Value Parser Input -> ParseResult)
;; [#:k (Value Input -> X)]
;; -> X
;; where X is usually ParseResult unless `k` is supplied.
(define (raw-pratt-parse tokens nud lbp led #:k [k values])
  (define (parse tokens rbp k)
    (define-values (first-left first-tokens) (nud parse tokens))
    (let loop ((left first-left) (tokens first-tokens))
      (match tokens
        [(cons t tokens) #:when (> (lbp t) rbp)
         (define-values (new-left new-tokens) (led t left parse tokens))
         (loop new-left new-tokens)]
        [_
         (k left tokens)])))
  (parse tokens 0 k))

;; (Value -> Value) -> (Value Input -> ParseResult)
(define (build-ast builder)
  (lambda (right tokens)
    (values (builder right) tokens)))

;; Symbol -> (Value Input -> Value)
;; Useful as a #:k argument to pratt-parse and friends.
(define (ensure-no-leftover-tokens who)
  (lambda (value remaining-tokens)
    (when (pair? remaining-tokens) (error who "Leftover tokens"))
    value))

;; Input
;; OperatorTable
;; OperatorTable
;; OperatorTable
;; Natural
;; (Value Value -> Value)
;; (-> Value)
;; [#:k (Value Input -> X)]
;; -> X
;; where X is usually Value unless `k` is supplied.
(define (pratt-parse tokens
                     prefix-tab
                     postfix-tab
                     infix-tab
                     adjacency-bp
                     compound?
                     handle-compound
                     handle-adjacency
                     handle-eof
                     #:k [k (ensure-no-leftover-tokens 'pratt-parse)])

  ;; Parser Input -> ParseResult
  (define (nud parse tokens) ;; "null denotation"
    (match tokens
      ['()
       (values (handle-eof) '())]
      [(cons t tokens)
       (if (compound? t)
           (let ((ts-val (handle-compound t (lambda (ts)
                                              (parse ts
                                                     0
                                                     (ensure-no-leftover-tokens 'pratt-parse))))))
             (values ts-val tokens))
           (match (prefix-tab t)
             [(operator _ bp associativity ctor)
              (case associativity
                [(prefix) (parse tokens bp (build-ast (lambda (v)
                                                        ;; (log-info "prefix ~v ~v" t v)
                                                        (ctor t v))))]
                [(prefix-macro)
                 ;; (log-info "prefix-macro ~v ~v" t tokens)
                 (ctor t parse tokens)]
                [else (error 'pratt-parse "Prefix use of non-prefix, non-prefix-macro operator ~v" t)])]
             [#f
              (values t tokens)]))]))

  ;; Token -> Natural
  (define (lbp t) ;; "left binding power"
    (cond [(infix-tab t) => operator-binding-power]
          [(postfix-tab t) => operator-binding-power]
          [else adjacency-bp]))

  ;; Token Value Parser Input -> ParseResult
  (define (led t left parse tokens) ;; "left denotation"
    (match (infix-tab t)
      [(operator _ bp (or 'left 'nonassoc 'n-ary) ctor)
       (parse tokens bp (build-ast (lambda (right)
                                     ;; (log-info "left/nonassoc/n-ary ~v ~v ~v" t left right)
                                     (ctor t left right))))]
      [(operator _ bp 'right ctor)
       (parse tokens (- bp 1) (build-ast (lambda (right)
                                           ;; (log-info "right ~v ~v ~v" t left right)
                                           (ctor t left right))))]
      [#f
       (match (postfix-tab t)
         [(operator _ bp _ ctor)
          (values (ctor t left) tokens)]
         [#f
          (parse (cons t tokens)
                 adjacency-bp
                 (build-ast (lambda (right)
                              ;; (log-info "adj ~v ~v" left right)
                              (handle-adjacency left right))))])]))

  (raw-pratt-parse #:k k tokens nud lbp led))

(module+ test
  (require rackunit)

  (define (finalize-app x)
    (match x
      [`(partial-app ,x) `(app ,x)]
      [_ x]))

  (define (mkunary ctor)
    (lambda (_t left) `(,ctor ,(finalize-app left))))

  (define (mkbinary ctor)
    (lambda (_t left right) `(,ctor ,(finalize-app left) ,(finalize-app right))))

  (define (handle-adjacency left right)
    (match left
      [`(partial-app ,xs) `(partial-app (,@xs ,right))]
      [f `(partial-app (,f ,right))]))

  (define ((assq-lookup tab) token)
    (cond [(assq token tab) => (lambda (entry) (apply operator entry))]
          [else #f]))

  (define (p tokens)
    (finalize-app
     (pratt-parse tokens
                  (assq-lookup
                   `((- 100 prefix ,(mkunary 'unary-minus))
                     (slurp 300 prefix-macro ,(lambda (t parse tokens) (values `(,t ,@tokens)
                                                                               '(z))))
                     (~ 300 prefix ,(mkunary 'tight-unary-minus))))
                  (assq-lookup
                   `((! 300 postfix ,(mkunary 'factorial))))
                  (assq-lookup
                   `((+ 20 left ,(mkbinary '+))
                     (- 20 left ,(mkbinary '-))
                     (* 30 left ,(mkbinary '*))
                     (/ 30 left ,(mkbinary '/))
                     (: 5 right ,(mkbinary 'cons))))
                  200
                  list?
                  (lambda (ts parse) (finalize-app (parse ts)))
                  handle-adjacency
                  (lambda () '()))))

  (check-equal? (p '(a b c + d e f * g h i))
                '(+ (app (a b c)) (* (app (d e f)) (app (g h i)))))

  (check-equal? (p '(a b c - d e f * g h i))
                '(- (app (a b c)) (* (app (d e f)) (app (g h i)))))

  (check-equal? (p '((a b c + d e f) * g h i))
                '(* (+ (app (a b c)) (app (d e f))) (app (g h i))))

  (check-equal? (p '(- a))
                '(unary-minus a))

  (check-equal? (p '(- a d))
                '(unary-minus (app (a d))))

  (check-equal? (p '(- a + b))
                '(+ (unary-minus a) b))

  (check-equal? (p '(b c * - a d))
                '(* (app (b c)) (unary-minus (app (a d)))))

  (check-equal? (p '(d (- a) b))
                '(app (d (unary-minus a) b)))

  (check-equal? (p '(- a ! + b))
                '(+ (unary-minus (factorial a)) b))

  (check-equal? (p '(- a + b !))
                '(+ (unary-minus a) (factorial b)))

  (check-equal? (p '(a b c ! d e f g h))
                '(app (a b (factorial c) d e f g h)))

  (check-equal? (p '(a b c ! d e f - g h))
                '(- (app (a b (factorial c) d e f)) (app (g h))))

  (check-equal? (p '(a b c ! d e f ~ g h))
                '(app (a b (factorial c) d e f (tight-unary-minus g) h)))

  (check-equal? (p '(a b c ! d e f slurp g h * foo ~ bar))
                '(app (a b (factorial c) d e f (slurp g h * foo ~ bar) z)))

  (check-equal? (p '(a)) 'a)
  (check-equal? (p '(a b)) '(app (a b)))
  (check-equal? (p '(a b c)) '(app (a b c)))
  (check-equal? (p '(a b c d)) '(app (a b c d)))

  (check-equal? (p '(1 : 2 : 3 : ()))
                `(cons 1 (cons 2 (cons 3 ()))))
  (check-equal? (p '(1 : 2 : 3 ! : ()))
                `(cons 1 (cons 2 (cons (factorial 3) ()))))
  (check-equal? (p '(a : f g h i : 3 ! : ()))
                `(cons a (cons (app (f g h i)) (cons (factorial 3) ()))))

  (check-equal? (p '(a (+) b))
                `(app (a + b)))

  (check-equal? (p '(-(a b)))
                `(unary-minus (app (a b))))
  (check-equal? (p '(x + -(a b)))
                `(+ x (unary-minus (app (a b)))))

  (check-equal? (p '((a b c) d e))
                '(app ((app (a b c)) d e)))
  )
