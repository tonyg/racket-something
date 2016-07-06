#lang something // -*- sth -*-
// Requires the "syndicate" package to be installed.

require
  something/infix
  "syndicate.rkt"

struct account [balance] :prefab
struct deposit [amount] :prefab

run-ground
  actor
    forever (balance 0)
      assert (account balance)
      on message (deposit $amount)
         balance + amount

  actor
    forever
      on asserted (account $balance)
         printf "Balance changed to ~a\n" balance

  actor
    until asserted (observe (deposit _))
    !! deposit +100
    !! deposit -30
