#lang something // -*- sth -*-

require
  for-syntax something/base
  only-in syndicate/actor \
    assert \
    message asserted retracted
  except-in syndicate \
    assert run-ground
  prefix-in s_ syndicate/actor
  prefix-in s_ syndicate/ground

provide
  for-syntax (all-defined-out)
  (all-defined-out)
  all-from-out syndicate
  all-from-out syndicate/actor

def-syntax def-block-syntax stx
  syntax-case stx []
    _ outer-id inner-id
      syntax (def-syntax outer-id outer-stx:
                syntax-case outer-stx [block]
                  _ (block body (... ...))
                    syntax (inner-id body (... ...)))

def-block-syntax actor s_actor
def-block-syntax run-ground s_run-ground

def-syntax forever stx
  syntax-case stx [block]
    _ (id init) ... (block body ...)
      syntax (s_forever :collect ((id init) ...) body ...)

def-operator !! 10 prefix !!
def !! v: s_send! v

begin-for-syntax
  def rewrite-ongoing-clauses parse stx
    let walk (stx stx)
      syntax-case stx [block]
        (:init (block expr ...)) rest ...
          (quasisyntax (:init (('#%rewrite-infix' expr) ...)
                        (unsyntax-splicing (walk (syntax (rest ...))))))
        (:done (block expr ...)) rest ...
          (quasisyntax (:done (('#%rewrite-infix' expr) ...)
                        (unsyntax-splicing (walk (syntax (rest ...))))))
        (:collect ((id init) ...)) rest ...
          (quasisyntax (:collect ((id ('#%rewrite-infix' init)) ...)
                        (unsyntax-splicing (walk (syntax (rest ...))))))
        clause rest ...
          (quasisyntax ((unsyntax (parse (syntax clause)))
                        (unsyntax-splicing (walk (syntax (rest ...))))))
        (: quasisyntax ())

def-operator until #f prefix-macro until
def-syntax until stx parse
  syntax-case stx [block]
    _ evt ... (block clause ...)
      (quasisyntax/loc stx
        (s_until (unsyntax (parse (syntax (evt ...))))
                 (unsyntax-splicing (rewrite-ongoing-clauses parse (syntax (clause ...))))))
    _ evt ...
      (quasisyntax/loc stx (s_until (unsyntax (parse (syntax (evt ...))))))

def-operator during #f prefix-macro during
def-syntax during stx parse
  syntax-case stx [block]
    _ pat ... (block clause ...)
      (quasisyntax/loc stx
        (s_during (unsyntax (parse (syntax (pat ...))))
                  (unsyntax-splicing (rewrite-ongoing-clauses parse (syntax (clause ...))))))

def-operator on #f prefix-macro on
def-syntax on stx parse
  syntax-case stx [block]
    _ evt ... (block expr ...)
      (quasisyntax/loc stx
        (s_on (unsyntax (parse (syntax (evt ...))))
              ('#%rewrite-infix' expr) ...))
