#lang something // -*- sth -*-

require
  for-syntax something/base
  something/infix
  rename-in (only-in racket/match match) (match base_match)

def-operator my-match #f prefix-macro my-match
def-syntax my-match stx (parse #f)
  syntax-case stx (block)
    _ subject ... (block (match-pat ... (block body ...)) ...)
      let
        log-info "my-match ~v" (syntax->datum . stx)
        with-syntax { parsed-subject: parse (syntax (subject ...))
                      (parsed-match-pat ...):
                        map parse (syntax->list . syntax ((match-pat ...) ...)) }
          log-info "parsed-subject ~v" (syntax->datum . syntax parsed-subject)
          log-info "parsed-match-pats ~v" (syntax->datum . syntax (parsed-match-pat ...))
          syntax (base_match parsed-subject (parsed-match-pat ('#%rewrite-body' body ...)) ...)

def-operator slurp1 #f prefix-macro slurp1
def-syntax slurp1 stx parse
  log-info "slurp1 invoked with stx ~v" (syntax->datum stx)
  syntax-case stx ()
    _ it rest ...
      let
        def the-rest: syntax (99 98 97 rest ...)
        log-info "slurp1 returning the-rest ~v" (syntax->datum the-rest)
        values (syntax (list (quote slurped) (quote it))) the-rest

(dump-operator-table!)

list 1 slurp1 x 65 64
list 1 slurp1 (x) 65 64
list 1 slurp1 (x y z) 65 64

{ v:: w: ["outer"; v; w] } . (my-match car . list 597 598 599 { zot: ["foo"; zot]; baz: ["xyzzy"; baz] }) . list 3 4

{ v: ["match-result"; v] } my-match car . list 97 98 99 { x: x }
  (list): "an empty list"
  (list v ...): ["nonempty list"; v]
  ? odd? x:
    log-info "got odd! ~v" x
    ["odd"; x]
  97: "ninety-seven"
  98: "ninety-eight"
  _: "idk"
