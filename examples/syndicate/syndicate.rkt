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
  syntax-case stx ()
    _ outer-id inner-id
      syntax (def-syntax outer-id outer-stx:
                syntax-case outer-stx (block)
                  _ (block body (... ...))
                    syntax (inner-id body (... ...)))

def-block-syntax spawn s_spawn
def-block-syntax spawn* s_spawn*
def-block-syntax run-ground s_run-ground
def-block-syntax forever s_forever
def-block-syntax react s_react
def-block-syntax begin/dataflow s_begin/dataflow
def-block-syntax on-start s_on-start
def-block-syntax on-stop s_on-stop

def-syntax def-event-syntax stx
  syntax-case stx ()
    _ outer-id inner-id
      syntax (begin (def-operator outer-id #f prefix-macro outer-id)
                    (def-syntax outer-id outer-stx parse:
                      syntax-case outer-stx (block):
                        _ evt (... ...) (block body (... ...))
                          (quasisyntax/loc outer-stx
                            (inner-id (unsyntax (parse (syntax (evt (... ...)))))
                                      (unsyntax-splicing
                                       (map parse (syntax->list (syntax (body (... ...))))))))
                        _ evt (... ...)
                          (quasisyntax/loc outer-stx
                            (inner-id (unsyntax (parse (syntax (evt (... ...)))))))))

def-event-syntax until s_until
def-event-syntax during s_during
def-event-syntax during/spawn s_during/spawn
def-event-syntax on s_on
def-event-syntax stop-when s_stop-when

def-syntax field stx
  syntax-case stx ()
    _ id (block init)
      syntax (s_field (id init))

def-syntax def/dataflow stx
  syntax-case stx ()
    _ id (block init)
      syntax (s_define/dataflow id init)

def-operator ! 1100 prefix !
def ! f: (f)

def-operator <- 10 nonassoc <-
def <- f v: f v

def-operator !! 10 prefix !!
def !! v: s_send! v

def-operator $ 1100 prefix $
