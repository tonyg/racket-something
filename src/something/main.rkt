#lang racket/base

(require (prefix-in base: racket/base))
(base:require "base.rkt" "infix.rkt" (only-in "lang/reader.rkt" read-toplevel-syntax))
(base:provide (base:all-from-out "base.rkt" "infix.rkt"))

(current-read-interaction read-toplevel-syntax)
