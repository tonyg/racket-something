#lang racket/base

(provide (except-out (all-from-out racket/base)
                     define
                     define-values
                     begin-for-syntax
                     define-syntax
                     syntax
                     require
                     provide
                     struct
                     syntax-case
                     with-syntax
                     let
                     let*
                     letrec
                     when
                     unless
                     cond
                     module+
                     if
                     #%top-interaction
                     with-handlers
                     parameterize)
         (except-out (all-from-out racket/match) match)
         #%seq
         block
         |.|
         def-operator
         get-operator-table!
         dump-operator-table!
         #%rewrite-infix
         #%no-infix
         (rename-out [something-define def]
                     [something-define-values def-values]
                     [something-begin-for-syntax begin-for-syntax]
                     [something-define-syntax def-syntax]
                     [something-require require]
                     [something-provide provide]
                     [something-match match]
                     [something-struct struct]
                     [something-syntax-case syntax-case]
                     [something-with-syntax with-syntax]
                     [something-syntax syntax]
                     [something-map map*]
                     [something-let let]
                     [something-let* let*]
                     [something-letrec letrec]
                     [something-when when]
                     [something-unless unless]
                     [something-cond cond]
                     [something-module+ module+]
                     [something-if if]
                     [something-top-interaction #%top-interaction]
                     [something-with-handlers with-handlers]
                     [something-parameterize parameterize]))

(require racket/match)
(require (only-in racket/list split-at-right))

(require (for-syntax racket))
(require (for-syntax syntax/stx))
(require (for-syntax syntax/id-table))
(require (for-syntax "pratt.rkt"))
(require (for-syntax syntax/srcloc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax operators
  (begin ;; (log-info "creating fresh operator table")
         (make-free-id-table)))

(define-for-syntax valid-associativities '(prefix prefix-macro postfix left right nonassoc n-ary))

(define-for-syntax (macro? stx)
  (and (identifier? stx)
       (syntax-local-value stx (lambda () #f))))

(define-syntax (#%no-infix stx)
  (raise-syntax-error #f "Use of #%no-infix survived infixification" stx))

(define-syntax (partial-app stx)
  (raise-syntax-error #f "Internal error: partially-built application escaped #%rewrite-infix" stx))

(define-for-syntax (->syntax ctx sexp)
  (datum->syntax ctx sexp ctx))

(define-for-syntax (finalize-app stx)
  (syntax-case stx (partial-app)
    [(partial-app . vals)
     #'vals]
    [_
     stx]))

(define-for-syntax (rewrite-infix stx)
  ;; (log-info "APP --> ~v" (syntax->datum stx))
  (define result
    (finalize-app
     (syntax-case stx (#%no-infix)
       [(#%no-infix term)
        (syntax/loc stx term)]
       [(special-id body ...)
        (and (identifier? #'special-id)
             (or (free-identifier=? #'special-id #'block)
                 (free-identifier=? #'special-id #'#%seq)))
        (->syntax stx (cons #'special-id (map rewrite-infix (syntax->list #'(body ...)))))]
       [(piece)
        (->syntax stx (list (rewrite-infix #'piece)))]
       [pieces
        (stx-pair? #'pieces)
        (let ((result (pratt-parse (syntax->list #'pieces)
                                   (operator-table '(prefix prefix-macro))
                                   (operator-table '(postfix))
                                   (operator-table '(left right nonassoc n-ary))
                                   1000
                                   (lambda (stx) (pair? (syntax->list stx)))
                                   (lambda (stx parse) (rewrite-infix stx))
                                   (lambda (left right)
                                     (syntax-case left (partial-app)
                                       [(partial-app . xs)
                                        (->syntax left `(,#'partial-app ,@(syntax->list #'xs) ,right))]
                                       [_
                                        (->syntax left `(,#'partial-app ,left ,right))]))
                                   (lambda () eof))))
          (if (eof-object? result)
              #''()
              result))
        ]
       [piece
        #'piece])))
  ;; (log-info "<-- APP ~v" (if (syntax? result) (syntax->datum result) result))
  result)

(define-for-syntax (same-binding-power? op-stx binding-power)
  (for/or [(op (in-set (free-id-table-ref operators op-stx set)))]
    (= (operator-binding-power op) binding-power)))

(define-for-syntax (finalize-unary-subterm handler)
  (lambda (t v) (handler t (finalize-app v))))
(define-for-syntax (finalize-binary-subterms handler)
  (lambda (t l r) (handler t (finalize-app l) (finalize-app r) l)))

(define-for-syntax (build-operator id-stx binding-power associativity handler-stx)
  (operator id-stx
            binding-power
            associativity
            (case associativity
              [(left right)
               (finalize-binary-subterms
                (lambda (op-stx left right _raw-left)
                  (if (eof-object? right)
                      (->syntax left (list left op-stx))
                      (->syntax handler-stx (list handler-stx left right)))))]
              [(nonassoc)
               (finalize-binary-subterms
                (lambda (op-stx left right raw-left)
                  (if (eof-object? right)
                      (->syntax left (list left op-stx))
                      (let ((result (->syntax left (list handler-stx left right))))
                        (syntax-case raw-left ()
                          [(op _ _)
                           (same-binding-power? #'op binding-power)
                           (raise-syntax-error
                            #f
                            (format "Cannot chain non-associative operators ~a and ~a"
                                    (syntax-e op-stx)
                                    (syntax-e #'op))
                            result)]
                          [_ result])))))]
              [(n-ary)
               (finalize-binary-subterms
                (lambda (op-stx left right raw-left)
                  (syntax-case raw-left ()
                    [(op args ...)
                     (eq? #'op handler-stx)
                     (if (eof-object? right)
                         (raise-syntax-error #f
                                             (format "Missing n-ary argument to operator ~a"
                                                     (syntax-e op-stx))
                                             left)
                         (->syntax left `(,handler-stx ,@(syntax->list #'(args ...)) ,right)))]
                    [(op args ...)
                     (same-binding-power? #'op binding-power)
                     (raise-syntax-error
                      #f
                      (format "Cannot chain non-associative operators ~a and ~a"
                              (syntax-e op-stx)
                              (syntax-e #'op))
                      (->syntax left (list handler-stx left right)))]
                    [_
                     (if (eof-object? right)
                         (->syntax left (list left op-stx))
                         (->syntax left (list handler-stx left right)))])))]
              [(prefix-macro)
               (lambda (op-stx parse tokens)
                 (define tokens-stx (->syntax op-stx (cons handler-stx tokens)))
                 (when (not (macro? handler-stx))
                   (raise-syntax-error #f
                                       (format "parser-macro handler is not a macro ~a"
                                               (syntax-e op-stx))
                                       tokens-stx))
                 (define (user-parse tokens-stx
                                     [rbp 0]
                                     [k (ensure-no-leftover-tokens 'prefix-macro-user-parse)])
                   (parse (syntax-case tokens-stx ()
                            [(token) (syntax->list #'(token))]
                            [(token ...) (syntax->list #'((token ...)))])
                          rbp
                          (lambda (v toks)
                            (k (finalize-app v) toks))))
                 (call-with-values
                  (lambda () ((syntax-local-value handler-stx) tokens-stx user-parse))
                  (case-lambda
                    [(value tokens) (values value (if (list? tokens) tokens (syntax->list tokens)))]
                    [(value) (values value '())])))]
              [(prefix postfix)
               (finalize-unary-subterm
                (lambda (op-stx v)
                  (->syntax v (list handler-stx v))))])))

(define-for-syntax (define-operator! id-stx binding-power-stx associativity-stx handler-stx)
  (define binding-power (syntax-e binding-power-stx))
  (define associativity (syntax-e associativity-stx))
  (case associativity
    [(prefix-macro)
     (when (not (eq? binding-power #f))
       (raise-syntax-error #f
                           (format "Binding power for prefix-macro must be #f; got ~v" binding-power)
                           binding-power-stx))]
    [else
     (when (not (and (integer? binding-power) (positive? binding-power)))
       (raise-syntax-error #f
                           (format "Binding power must be positive integer; got ~v" binding-power)
                           binding-power-stx))])
  (when (not (memq associativity valid-associativities))
    (raise-syntax-error #f
                        (format "Associativity must be one of ~a; got ~v"
                                valid-associativities
                                associativity)
                        associativity-stx))
  (define op (build-operator id-stx binding-power associativity handler-stx))
  (define associativities-to-clear
    (case associativity
      [(prefix prefix-macro) '(prefix prefix-macro)]
      [(postfix) '(postfix)]
      [(left right nonassoc n-ary) '(left right nonassoc n-ary)]))
  (define existing-set
    (for/set [(op (in-set (free-id-table-ref operators id-stx set)))
              #:when (not (memq (operator-associativity op) associativities-to-clear))]
      op))
  ;; (log-info "defining operator ~v" id-stx)
  (free-id-table-set! operators
                      id-stx
                      (set-add existing-set op)))

(define-for-syntax (undefine-operator! id-stx)
  ;; (log-info "undefining operator ~v" id-stx)
  (free-id-table-remove! operators id-stx))

(define-syntax-rule (get-operator-table! fn)
  (begin-for-syntax (fn operators)))

(define-syntax (dump-operator-table! stx)
  (syntax-case stx ()
    [(_)
     #`(get-operator-table!
        (lambda (table)
          (local-require racket/base)
          (local-require racket/pretty)
          (eprintf "~aOperator table:\n" #,(source-location->prefix stx))
          (free-id-table-for-each
           table
           (lambda (id ops)
             (for [(op (in-set ops))]
               (match-define (operator _ bp associativity _) op)
               (eprintf "  ~a\t~a\t~a\t~a\n"
                        bp
                        associativity
                        (syntax-e id)
                        (source-location->string id)))))))]))

(define-for-syntax (operator-table associativities)
  (lambda (id-stx)
    ;; (log-info " - looking up ~v for ~v" id-stx associativities)
    (and (identifier? id-stx)
         (let ((ops (free-id-table-ref operators id-stx set)))
           (for/or [(op (in-set ops))]
             ;; (log-info "   - comparing ~v with ~v" id-stx op)
             (and op (memq (operator-associativity op) associativities) op))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (#%rewrite-infix stx)
  ;; (log-info "#%rewrite-infix(~v) --> ~v" (syntax-local-phase-level) stx)
  (define result
    (syntax-case stx ()
      [(_ token) (rewrite-infix #'token)]
      [(_ . tokens) (rewrite-infix #'tokens)]))
  ;; (log-info "<-- #%rewrite-infix(~v) ~v" (syntax-local-phase-level) (syntax->datum result))
  result)

(define-syntax (def-operator stx [parse #f]) ;; optional parse -> can be used in both contexts
  (syntax-case stx ()
    [(_ id binding-power associativity handler)
     #'(begin-for-syntax (define-operator! #'id #'binding-power #'associativity #'handler))]
    [(_ id)
     #'(begin-for-syntax (undefine-operator! #'id))]))
(def-operator def-operator #f prefix-macro def-operator)

(define-match-expander #%seq
  (syntax-id-rules ()
    [(_ pat ...) (list pat ...)]
    [_ (list)])
  (syntax-id-rules ()
    [(_ exp ...) (#%app list exp ...)]
    [_ (#%app list)]))

(define-syntax (block stx)
  (syntax-case stx ()
    [(_ clause ...)
     (with-syntax ([(transformed-clause ...)
                    (map (lambda (clause-stx)
                           (syntax-case clause-stx (block)
                             [(pat ... (block body ...))
                              #'[(list pat ...) body ...]]
                             [(block body ...)
                              #'[(list) body ...]]))
                         (syntax->list #'(clause ...)))])
       #'(match-lambda* transformed-clause ...))]))

(def-operator |.| 900 left |.|)
(define-syntax-rule (|.| f v) (f v))

(def-operator something-define #f prefix-macro something-define)
(require (for-syntax syntax/strip-context))
(define-syntax (something-define stx [parse #f])
  (syntax-case stx (block)
    [(_ (f v ...) (block body ...))
     #'(define (f v ...) (#%rewrite-infix body) ...)]
    [(_ f v0 v ... (block body ...))
     #'(define (f v0 v ...) (#%rewrite-infix body) ...)]
    [(_ v (block e))
     #'(define v (#%rewrite-infix e))]))

(define-syntax (something-define-values stx)
  (syntax-case stx (block)
    [(_ var ... (block body ...))
     #'(define-values (var ...) (let () body ...))]))

(def-operator something-begin-for-syntax #f prefix-macro something-begin-for-syntax)
(define-syntax (something-begin-for-syntax stx parse)
  (syntax-case stx (block)
    [(_ (block body ...))
     (quasisyntax/loc stx
       (begin-for-syntax (#,(syntax-shift-phase-level #'#%rewrite-infix 1) body) ...))]))

(def-operator something-define-syntax #f prefix-macro something-define-syntax)
(define-syntax (something-define-syntax stx [parse #f])
  (syntax-case stx (block)
    [(_ f v0 v ... (block body ...))
     (quasisyntax/loc stx
       (define-syntax (f v0 v ...)
         (#,(syntax-shift-phase-level #'#%rewrite-infix 1) body) ...))]
    [(_ v e)
     (quasisyntax/loc stx
       (define-syntax v
         (#,(syntax-shift-phase-level #'#%rewrite-infix 1) e)))]))

(def-operator something-require #f prefix-macro something-require)
(define-syntax (something-require stx [parse #f])
  ;; optional "parse", making this a dual macro, e.g. for support for
  ;; simple .racketrc/interactive usage
  (syntax-case stx (block require)
    [(_ w ... (block v ...))
     #'(require w ... v ...)]
    [(_ w ...)
     #'(require w ...)]))

(def-operator something-provide #f prefix-macro something-provide)
(define-syntax (something-provide stx [parse #f])
  (syntax-case stx (block)
    [(_ w ... (block v ...))
     #'(provide w ... v ...)]
    [(_ w ...)
     #'(provide w ...)]))

(def-operator something-match #f prefix-macro something-match)
(define-syntax (something-match stx parse)
  (syntax-case stx (block)
    [(_ e ... (block (pat-piece ... (block body ...)) ...))
     #`(match (#%rewrite-infix e ...)
         #,@(map (lambda (clause-stx)
                   (syntax-case clause-stx ()
                     [((p ...) (body ...))
                      #`[#,(parse #'(p ...))
                         (#%rewrite-infix body) ...]]))
                 (syntax->list #'([(pat-piece ...) (body ...)] ...))))]))

(define-syntax (something-struct stx)
  (syntax-case stx (#%seq block)
    [(_ name super (#%seq field ...) (block rest ...))
     #'(struct name super (field ...) rest ...)]
    [(_ name super (#%seq field ...) rest ...)
     #'(struct name super (field ...) rest ...)]
    [(_ name (#%seq field ...) (block rest ...))
     #'(struct name (field ...) rest ...)]
    [(_ name (#%seq field ...) rest ...)
     #'(struct name (field ...) rest ...)]))

(def-operator something-syntax-case #f prefix-macro something-syntax-case)
(define-syntax (something-syntax-case stx parse)
  (syntax-case stx (#%seq block)
    [(_ s (#%seq lit ...) (block (pat ... (block body ...)) ...))
     (quasisyntax/loc stx
       (syntax-case s (lit ...)
         #,@(map (lambda (clause-stx)
                   (syntax-case clause-stx ()
                     [((pat ...) (body ...))
                      #`(#,(parse #'(pat ...)) #,@(map parse (syntax->list #'(body ...))))]))
                 (syntax->list #'([(pat ...) (body ...)] ...)))))]))

(def-operator something-with-syntax #f prefix-macro something-with-syntax)
(define-syntax (something-with-syntax stx parse)
  (syntax-case stx (block)
    [(_ (block (pattern (block stx-expr)) ...) (block body ...))
     #'(with-syntax ([pattern (#%rewrite-infix stx-expr)] ...) (#%rewrite-infix body) ...)]))

(def-operator something-syntax #f prefix-macro something-syntax)
(define-syntax (something-syntax stx parse)
  (syntax-case stx (block)
    [(_ template)
     (syntax/loc stx (syntax template))]))

(define (something-map arg . args)
  (define-values (lists f-list) (split-at-right (cons arg args) 1))
  (apply map (car f-list) lists))

(define-for-syntax (expand-let-like kind-stx expr-stx)
  (syntax-case expr-stx (block)
    [(_ loop (name init) ... (block body ...))
     (identifier? #'loop)
     #`(#,kind-stx loop ((name init) ...) body ...)]
    [(_ (name init) ... (block body ...))
     #`(#,kind-stx ((name init) ...) body ...)]))

(define-syntax (something-let stx) (expand-let-like #'let stx))
(define-syntax (something-let* stx) (expand-let-like #'let* stx))
(define-syntax (something-letrec stx) (expand-let-like #'letrec stx))

(def-operator something-when #f prefix-macro something-when)
(define-syntax (something-when stx parse)
  (syntax-case stx (block)
    [(something-when test ... (block body ...))
     (syntax/loc stx (when (#%rewrite-infix test ...) (#%rewrite-infix body) ...))]))

(def-operator something-unless #f prefix-macro something-unless)
(define-syntax (something-unless stx parse)
  (syntax-case stx (block)
    [(something-unless test ... (block body ...))
     (syntax/loc stx (unless (#%rewrite-infix test ...) (#%rewrite-infix body) ...))]))

(def-operator something-cond #f prefix-macro something-cond)
(define-syntax (something-cond stx parse)
  (syntax-case stx (block something-when something-unless else)
    [(_ (block (something-when test ... (block body ...)) clauses ...))
     (syntax/loc stx (if (#%rewrite-infix test ...)
                         (begin (#%rewrite-infix body) ...)
                         (#%rewrite-infix (something-cond (block clauses ...)))))]
    [(_ (block (something-unless test ... (block body ...)) clauses ...))
     (syntax/loc stx (if (not (#%rewrite-infix test ...))
                         (begin (#%rewrite-infix body) ...)
                         (#%rewrite-infix (something-cond (block clauses ...)))))]
    [(_ (block (else (block body ...))))
     (syntax/loc stx (begin (#%rewrite-infix body) ...))]
    [(_ (block))
     (raise-syntax-error #f "cond: no matching clause" stx)]))

(define-syntax (something-module+ stx)
  (syntax-case stx (block)
    [(something-module+ modname (block body ...))
     (syntax/loc stx (module+ modname body ...))]))

(def-operator something-if #f prefix-macro something-if)
(define-syntax (something-if stx parse)
  (syntax-case stx (block)
    [(something-if test ... (block then else))
     (syntax/loc stx (if (#%rewrite-infix (test ...)) (#%rewrite-infix then) (#%rewrite-infix else)))]))

(define-syntax (something-top-interaction stx)
  (syntax-case stx ()
    [(_ . form)
     #'(#%rewrite-infix form)]))

(def-operator something-with-handlers #f prefix-macro something-with-handlers)
(define-syntax (something-with-handlers stx [parse #f])
  (syntax-case stx (block something-when)
    [(_ (block (something-when test-proc id (block handler ...)) ... body))
     (syntax/loc stx
       (with-handlers [((#%rewrite-infix test-proc)
                        (lambda (id) (#%rewrite-infix handler) ...)) ...]
         (#%rewrite-infix body)))]))

(define-syntax (something-parameterize stx)
  (syntax-case stx (#%seq block)
    [(_ (#%seq p ...) (block body ...))
     (quasisyntax/loc stx
       (parameterize (p ...) body ...))]))
