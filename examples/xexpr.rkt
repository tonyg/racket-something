#lang something // -*- sth -*-

require
  racket/pretty
  something/infix
  for-syntax something/base
  except-in xml document

def-syntax single-xexpr stx
  syntax-case stx [= , block]
    _ str
      string? (syntax-e . (syntax str))
      syntax str
    _ (= expr)
      syntax expr
    _ (tag (attr attr-expr) ... (block xexpr ...))
      syntax (list (quote tag) (list (list (quote attr) attr-expr) ...) (single-xexpr xexpr) ...)
    _ (tag (attr attr-expr) ...)
      syntax (list (quote tag) (list (list (quote attr) attr-expr) ...))

def-syntax xexpr stx
  syntax-case stx [block]
    _ (block xexpr)
      syntax (single-xexpr xexpr)

def-operator ++ 50 left string-append
def-operator = 10 prefix =

def document
  xexpr
    html
      head
        meta (http-equiv "Content-Type") (content "text/html; charset=utf-8")
        title: "Test page"
      body
        h1: "Hello"
        p: "Hello, world"
        h2: "Testing"
        p
          = "Hello, " ++ number->string (3 + 4)
          "! This rules."
        p
          "Another way of putting it would be to say that 3 + 4 = "
          = (number->string (3 + 4))
          "."

pretty-print document
printf "\n~a\n" (xexpr->string document)
