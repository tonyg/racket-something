#lang something // -*- sth -*-

provide
  rename-out (s2minus -)

require
  something/infix

def-operator +

def plus: +

def-operator + 60 left
  a b:
    log-info "plus-op ~v ~v" a b
    plus a b

def-operator s2minus 60 left s2minus

def s2minus a b
  log-info "s2minus ~v ~v" a b
  a - b
