#lang something // -*- sth -*-

provide
  IF

require
  something/infix
  prefix-in base_ racket/base
  for-syntax something/base

def-operator IF #f statement-macro IF-statement
def-syntax IF-statement stx
  syntax-case stx (ELSE block)
    _ (head ...) ((_IF test (block true-exps ...)) (ELSE (block false-exps ...)) more ...)
      syntax ('#%rewrite-body*' (head ... (base_cond (test ('#%rewrite-body' true-exps ...))
                                                     (else ('#%rewrite-body' false-exps ...))))
                                (more ...))
    _ (head ...) ((_IF test (block true-exps ...)) more ...)
      syntax ('#%rewrite-body*' (head ... (base_when test ('#%rewrite-body' true-exps ...)))
                                (more ...))
    _ (head ...) ((_IF pieces ...) more ...)
      syntax ('#%rewrite-body*' (head ...) ((begin (IF pieces ...)) more ...))

def-syntax IF stx
  syntax-case stx (ELSE block)
    _ test (block true-exps ...) ELSE (block false-exps ...)
      syntax (base_if test ('#%rewrite-body' true-exps ...) ('#%rewrite-body' false-exps ...))
    _ test (block true-exps ...)
      syntax (base_when test ('#%rewrite-body' true-exps ...))

module+ main

  printf "What's the magic word? "
  (flush-output)

  def word: (read-line)

  IF (word == "please")
     printf "You said the magic word!\n"
     printf "How polite.\n"
  ELSE
     printf "You didn't say the magic word.\n"

  def is_magic: word == "magic"

  IF is_magic
     printf "But you did say 'magic'!\n"

  IF is_magic { printf "✓\n" } ELSE { printf "✗\n" }
  displayln (IF (word == "1") { "✓" } ELSE { "✗" })
  displayln (IF (word == "1") { "✓" })
