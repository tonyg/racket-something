#lang something // -*- sth -*-

provide
  for
  for*
  for/list
  for*/list
  for/hash
  for*/hash

require
  something/infix
  for-syntax something/base
  prefix-in base_ racket/base

begin-for-syntax
  def expand-for base-stx stx
    syntax-case stx [block]
      _ (block (v (block exp)) ...) (block body ...)
        (quasisyntax ((unsyntax base-stx) ((v exp) ...) ('#%rewrite-body' body ...)))

def-syntax for stx: expand-for (syntax base_for) stx
def-syntax for* stx: expand-for (syntax base_for*) stx
def-syntax for/list stx: expand-for (syntax base_for/list) stx
def-syntax for*/list stx: expand-for (syntax base_for*/list) stx
def-syntax for/hash stx: expand-for (syntax base_for/hash) stx
def-syntax for*/hash stx: expand-for (syntax base_for*/hash) stx

def-operator .. 10 nonassoc in-range
def-operator .. 10 prefix in-range
