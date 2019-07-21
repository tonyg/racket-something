#lang something // -*- sth -*-

provide
  where

require
  something/infix
  for-syntax something/base

def-operator where #f statement-macro where
def-syntax where stx
  syntax-case stx (block)
    _ (head ...) ((_where (block (defhead ... (block defbody ...)) ...)))
      syntax (let (block (def defhead ... (block defbody ...)) ... head ...))

module+ main
  displayln (x + y x)
  where
    x: 123
    y z: z * 2
