#lang racket/base

(provide (rename-out [read-something-syntax read-syntax])
         read-toplevel-syntax)

(require (only-in parser-tools/lex position-line position-col position-offset))
(require syntax/strip-context)
(require racket/match)

(require "../reader.rkt")

(define (split-by pred xs)
  (define (consume-separators xs)
    (match xs
      ['() '()]
      [(cons x xs) #:when (pred x) (consume-separators xs)]
      [_ (gather-until-separator xs '())]))
  (define (gather-until-separator xs acc-rev)
    (match xs
      ['() (list (reverse acc-rev))]
      [(cons x xs) #:when (pred x) (cons (reverse acc-rev) (consume-separators xs))]
      [(cons x xs) (gather-until-separator xs (cons x acc-rev))]))
  (consume-separators xs))

(define (form->syntax src t)
  (define (->syntax v pos)
    (datum->syntax #f v (vector src
                                (position-line pos)
                                (position-col pos)
                                (position-offset pos)
                                #f)))
  (define (walk-form kids pos)
    (->syntax (map walk kids) pos))
  (define (walk t)
    (match t
      [(list kid)
       (walk kid)]
      [(list kids ...)
       (walk-form kids (if (pair? kids) (token-pos (car kids)) #f))]
      [(token pos 'form kids)
       (walk-form kids pos)]
      [(token pos 'block kids)
       (->syntax (cons #'block (map walk kids)) pos)]
      [(token pos 'sequence kids)
       (->syntax (cons #'#%seq (map walk (split-by token-comma? kids))) pos)]
      [(token pos _ (namespaced-name #f id))
       (->syntax id pos)]
      [(token pos _ (namespaced-name ns id))
       (->syntax (list #'in-module (->syntax ns pos) (->syntax id pos)) pos)]
      [(token pos 'keyword val)
       (->syntax (string->keyword (symbol->string val)) pos)]
      [(token pos (or 'number 'string 'literal) val)
       (->syntax val pos)]))
  (walk t))

(define (read-something-syntax src [p (current-input-port)] #:language [language #'something/base])
  (define forms (read-something-forms p))
  (strip-context
   #`(module something-module #,language
       (#%rewrite-body #,@(map (lambda (f) (form->syntax src f)) forms)))))

(define (read-toplevel-syntax src [p (current-input-port)])
  (define forms (read-something-toplevel p))
  (if (null? forms)
      eof
      (strip-context
       #`(#%rewrite-body #,@(map (lambda (f) (form->syntax src f)) forms)))))
