#lang something // -*- sth -*-

require
  something/for

///////////////////////////////////////////////////////////////////////////
// http://docs.racket-lang.org/honu/Examples.html

// A for loop that iterates between two bounds.
for { x: 1 + 5 .. 10 }
  printf "x is ~a\n" x

// Similar to above but shows a block of expressions in the body
for { x: 1 .. 10 }
  def y: x + 1
  printf "x ~a y ~a\n" x y

// A for loop that iterates over a list of numbers
for { x: [1, 2, 3] }
  printf "x ~a\n" x

///////////////////////////////////////////////////////////////////////////
// Some more examples

(newline)
for { x: .. 3; y: ["hello", "there", "racketeer"] }
  printf "~a ~a\n" x y

(newline)
for* { x: .. 3; y: ["hello", "there", "racketeer"] }
  printf "~a ~a\n" x y

for/hash { x : .. 3 }
  values x (x * 2)
