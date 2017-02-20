#lang something // -*- sth -*-
// Requires the "syndicate" package to be installed.

require
    something/infix
    "syndicate.rkt"

struct account [balance] :prefab
struct deposit [amount] :prefab

run-ground
    spawn
        field balance: 0
        assert (account !balance)
        on message (deposit $amount)
             balance <- !balance + amount

    spawn
        on asserted (account $balance)
             printf "Balance changed to ~a\n" balance

    spawn*
        until asserted (observe (deposit _))
        !! deposit +100
        !! deposit -30
