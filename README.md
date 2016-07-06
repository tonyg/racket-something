# Indentation-based Racket Syntax

Not [sweet-exps](http://readable.sourceforge.net/) (see Asumu's
[racket implementation](https://github.com/takikawa/sweet-racket)).
Not [srfi-49](http://srfi.schemers.org/srfi-49/srfi-49.html). More
inspired by Python and Haskell.

(The name "something" is temporary, and likely to be repurposed.)

# Installation

Check out the repository. Then, in the directory containing
`Makefile`,

    make link

or

	raco pkg install --link -n something `pwd`/src

# The main idea

S-expressions, but with usually-implicit parentheses. Indentation for
grouping is explicitly represented in the S-expression returned from
the reader.

This program:

    #lang something
    for { x: 1 .. 10 }
      def y: x + 1
      printf "x ~a y ~a\n" x y

... reads as this S-expression:

    (module something-module something/lang/implicit
      (#%rewrite-infix
       (for (block (x (block (1 .. 10))))
            (block (def y (block (x + 1)))
                   (printf "x ~a y ~a\n" x y)))))

The `#%rewrite-infix` macro consults an operator table, extendable via
the `def-operator` macro, to rewrite infix syntax into standard prefix
S-expressions.

The `block` syntax has many different interpretations. It has a macro
binding that turns it into a Racket `match-lambda*`, and it is used as
literal syntax as input to other macro definitions.

For example, here's one possible implementation of that `for` syntax:

    #lang something

    provide
      for

    require
      for-syntax something/lang/implicit
      prefix-in base_ racket/base

    def-syntax for stx
      syntax-case stx [block]
        _ (block (v (block exp)) ...) (block body ...)
          (syntax (base_for ((v exp) ...) body ...))

    def-operator .. 10 nonassoc in-range

Notice how the `block` S-expressions are rewritten into a normal
S-expression compatible with the underlying `for` from `racket/base`.

Generally, all of these forms are equivalent

    x y z          x y z:          x y z { a; b }
      a              a
      b              b

and they are read as

    (x y z (block a b))

and are then made available to the normal macro-expansion process
(which involves a new infix-rewriting semi-phase).

Colons are optional to indicate a following suite at the end of an
indentation-sensitive line. Indentation-sensitivity is disabled inside
parentheses and square brackets. If inside a parenthesised expression,
indentation-sensitivity can be reenabled with a colon at the end of a
line:

    a b (c d:
          e
          f)

    = (a b (c d (block e f)))

    a b (c d
          e
          f)

    = (a b (c d e f))

Conversely, long lines may be split up and logically continued over
subsequent physical lines with a trailing `\`:

    a b c \
      d \
      e

    = (a b c d e)

Semicolons may also appear in vertically-laid-out suites; these two
are equivalent:

    x y z
      a
      b; c
      d

    x y z { a; b; c; d }

Suites may begin on the same line as their colon. Any indented
subsequent lines become children of the portion after the colon,
rather than the portion before.

This example:

    x y z: a b
      c d
      e

reads as

    (x y z (block (a b (block (c d) e))))

Square brackets are syntactic sugar for a `#%seq` macro:

    [a, b, c, d]        →        (#%seq a b c d)

Forms starting with `block` in expression context expand into
`match-lambda*` like this:

    {
      pat1a pat1b
        exp1a
        exp1b
      pat2a
        exp2
    }

    → (match-lambda*
        [(list pat1a pat1b) exp1a exp1b]
        [(list pat2a) exp2]

The `map` function exported from `something/lang/implicit` differs
from that of `racket/base` in that it takes its arguments in the
opposite order, permitting maps to be written

    map [1, 2, 3, 4]
      item:
        item + 1

    map [1, 2, 3, 4]: item: item + 1

    map [1, 2, 3, 4] { item: item + 1 }

A nice consequence of all of the above is that curried functions have
an interesting appearance:

    def curried x:: y:: z:
      [x, y, z]

    require: rackunit
    check-equal? (((curried 1) 2) 3) [1, 2, 3]

# A larger example

More examples can be found in the [examples](examples/) and
[src/something/test](src/something/test/) directories.

    #lang something

    require
      racket/pretty
      something/infix
      for-syntax something/lang/implicit
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

# A note on lexical syntax

The lexical syntax of this reader is not exactly that of Racket. For
example, comments start with `//` rather than `;`, and the set of
allowable non-escaped identifiers is different (smaller).

I will likely revise this decision to bring it to be much closer to
Racket's lexical syntax.

# Emacs mode

See [sth8.el](sth8.el).

# Licence

Copyright (C) 2016 Tony Garnock-Jones <mailto:tonyg@leastfixedpoint.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program (see the files "lgpl.txt" and
"gpl.txt"). If not, see <http://www.gnu.org/licenses/>.
