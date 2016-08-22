#lang something // -*- sth -*-

require
  something/infix
  rename-in (only-in racket/base -) (- base-minus)
  "s2.rkt"
  rackunit

// (dump-operator-table!)

def print-and-return x
  printf "print-and-return ~a\n" x
  x

def check-output expected-output expected-logging f
  local-require (only-in racket/string string-split)
  local-require (only-in racket/logging with-logging-to-port)
  def-values actual-output actual-logging:
    def o: (open-output-string)
    def l: (open-output-string)
    parameterize [current-output-port o]: with-logging-to-port l f `info
    values (get-output-string o) (get-output-string l)
  check-equal? (string-split actual-output "\n") expected-output
  check-equal? (string-split actual-logging "\n") expected-logging

check-output (list "print-and-return #f"
                   "print-and-return 2"
                   "n is 0"
                   "n is 1"
                   "n is 2"
                   "n is 3"
                   "n is 4"
                   "n is 5"
                   "n is 6"
                   "n is 7"
                   "n is 8"
                   "n is 9"
                   "x in let is 123") \
             (list "plus-op 1 6"
                   "plus-op 3 4"
                   "plus-op 7 5"
                   "s2minus 3 4"
                   "s2minus -1 5"
                   "plus-op 1 2"
                   "plus-op 0 1"
                   "plus-op 1 1"
                   "plus-op 2 1"
                   "plus-op 3 1"
                   "plus-op 4 1"
                   "plus-op 5 1"
                   "plus-op 6 1"
                   "plus-op 7 1"
                   "plus-op 8 1"
                   "plus-op 9 1") \
             ::
  check-equal? (1 + 2 * 3) 7
  check-equal? (+ 3 4 5) 12
  check-equal? (3 + 4 + 5) 12
  check-equal? (3 - 4 - 5 base-minus 2 * 2) -10
  check-equal? (car [+]) (values +)
  check-equal? [1, + , 2, 1 + 2] (list 1 (values +) 2 3)
  check-equal? (2 > 1) #t

  check-equal? (print-and-return #f || print-and-return 2 || print-and-return 3) 2

  def let-test-result:
    let (x 123) (y 234) (z 345)
      [`inside-the-let, x, y, z]
  check-equal? let-test-result (list `inside-the-let 123 234 345)

  let loop (n 0)
    def still-running?: n < 10
    when still-running?
      printf "n is ~a\n" n
      loop (n + 1)

  def cond-result
    cond
      when #t
        #t
      else
        #f
  check-true cond-result

  def my-list
    { items ... : items }

  check-equal? (my-list 1 2 3 4) [1, 2, 3, 4]

  let
    def x: 123
    printf "x in let is ~a\n" x
    check-equal? x 123

  def curried x:: y:: z:
    [`curried, x, y, z]

  def ((curried2 x) y) z
    [`curried2, x, y, z]

  check-equal? (((curried 1) 2) 3) [`curried, 1, 2, 3]
  check-equal? (curried 1 . 2 . 3) [`curried, 1, 2, 3]
  check-equal? (((curried2 1) 2) 3) [`curried2, 1, 2, 3]
  check-equal? (curried2 1 . 2 . 3) [`curried2, 1, 2, 3]

def-operator > 40 n-ary >
check-equal? (4 > 3 > 2) #t
