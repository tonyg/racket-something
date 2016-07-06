#lang something // -*- sth -*-

def-operator + 60 left +
123 + 234

def call f: (f)

call ::
  printf "hi\n"
  printf "there\n"
  "result"

call { : printf "eek1\n" }
call . { : printf "eek2\n" }
