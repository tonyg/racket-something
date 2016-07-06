#lang something // -*- sth -*-
// Demonstrate sending a message to multiple receivers.
// Requires the "syndicate" package to be installed.

require
  something/infix
  "syndicate.rkt"

struct envelope [destination, message] :prefab

run-ground
  actor: forever: on message (envelope `alice $message)
                     log-info "Alice received ~v" message

  actor: forever: on message (envelope `bob $message)
                     log-info "Bob received ~v" message

  actor
    log-info "Waiting for Alice and Bob."
    until asserted (observe (envelope `alice _))
    until asserted (observe (envelope `bob _))

    log-info "Sending a few messages..."
    !! envelope `alice "For Alice's eyes only"
    !! envelope `bob "Dear Bob, how are you? Kind regards, etc."
    !! envelope ? "Important announcement!"

    log-info "Sent all the messages."
