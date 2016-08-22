#lang something // -*- sth -*-

require
  something/infix

log-info "Hello, world"
log-info "This is cool ~v" \
  "isn't it"

def f x
  x + 1

def g
  { x: x + 1 }

[f 123, f 124, g 125]

map* [f 123, f 124, g 125] f

map* [f 123, f 124, g 125]
  125: `one-two-five-times-two
  x: x * 2

map* [f 123, f 124, g 125]: x: x * 2

map* [f 123, f 124, g 125] { x: x * 2 }

printf "Type a Racket term: "
(flush-output)
match (read)
  [x, y]: x + y
  `hi: "Hello"
  `bye: "Goodbye"
  x: format "Something else: ~a" x
