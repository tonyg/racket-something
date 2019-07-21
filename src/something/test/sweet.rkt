#lang something // -*- sth -*-
// c.f. Sweet-expression examples
// https://sourceforge.net/p/readable/wiki/Solution/#quick-examples

require
  something/infix
  something/for

def fibfast n
  if n < 2
    n
    fibup n 2 1 0

def fibup max count n1 n2
  if max = count
    n1 + n2
    fibup max (count + 1) (n1 + n2) n1

def factorial n
  if n <= 1
    1
    n * factorial (n - 1)

module+ test
  require: rackunit
  check-equal? (for/list { x: .. 8 }: fibfast x) [0; 1; 1; 2; 3; 5; 8; 13]
  check-equal? (for/list { x: .. 8 }: factorial x) [1; 1; 2; 6; 24; 120; 720; 5040]

///////////////////////////////////////////////////////////////////////////

require
  for-syntax something/base

def-syntax sweet-let* stx // like the example at https://sourceforge.net/p/readable/wiki/Examples/
                          // but altered for something-style blocks
  syntax-case stx (block)
    _ (block body ...)
      syntax (begin body ...)
    _ (var init) rest ... (block body ...)
      syntax (let (var init) (block (sweet-let* rest ... (block body ...))))

module+ test
  def (t)
    def x: 0
    sweet-let* (x (x + 1)) \
               (x (x + 1)) \
               (x (x + 1)) \
               (x (x + 1))
      x

  check-equal? (t) 4

///////////////////////////////////////////////////////////////////////////

def add-if-all-numbers lst
  call/ec
    exit
      let loop (lst lst)
        if null? lst
          0
          if not.number? (car lst)
            exit #f
            car lst + loop (cdr lst)

def add-if-all-numbers2 lst
  call/ec: exit
    let loop (lst lst)
      if null? lst
        0
        if not (number? (car lst))
          exit #f
          car lst + loop (cdr lst)

def add-if-all-numbers3 lst
  call/ec: exit
    let loop (lst lst)
      cond
        when null? lst           : 0
        unless number? (car lst) : exit #f
        else                     : car lst + loop (cdr lst)

def add-if-all-numbers/acc lst
  let loop (lst lst) (sum 0)
    if null? lst
      sum
      if not (number? (car lst))
        #f
        loop (cdr lst) (sum + car lst)

def add-if-all-numbers/acc2 lst
  let loop (lst lst) (sum 0)
    cond
      when null? lst           : sum
      unless number? (car lst) : #f
      else                     : loop (cdr lst) (sum + car lst)

module+ test
  def check-adder adder
    check-equal? (adder [1; 2; 3; 4]) 10
    check-equal? (adder [1; 2; "hello"; 4]) #f

  check-adder add-if-all-numbers
  check-adder add-if-all-numbers2
  check-adder add-if-all-numbers3
  check-adder add-if-all-numbers/acc
  check-adder add-if-all-numbers/acc2
