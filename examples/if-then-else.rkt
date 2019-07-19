#lang something // -*- sth -*-

provide
  IF

require
  something/infix
  prefix-in base_ racket/base
  for-syntax something/base

def-operator IF #f statement-macro IF
def-syntax IF stx
  syntax-case stx [ELSE, ELIF, block]
    _ (_IF test (block true-exps ...)) (ELSE (block false-exps ...)) more ...
      syntax ('#%rewrite-body'
              (base_cond (test ('#%rewrite-body' true-exps ...))
                         (else ('#%rewrite-body' false-exps ...)))
              more ...)
    _ (_IF test (block true-exps ...)) more ...
      syntax ('#%rewrite-body'
              (base_when test ('#%rewrite-body' true-exps ...))
              more ...)

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
