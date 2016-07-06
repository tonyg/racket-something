#lang something // -*- sth -*-
// Toy file system, based on the example in the ESOP2016 submission.
// syndicate/actor implementation, using "during" instead of "on asserted/until retracted".
// Requires the "syndicate" package to be installed.

require
  something/infix
  "syndicate.rkt"
  syndicate/drivers/timer
  only-in racket/port read-bytes-line-evt
  only-in racket/string string-trim string-split

struct file [name, content] :prefab
struct save [file] :prefab
struct delete [name] :prefab

def sleep sec
  def timer-id: gensym `sleep
  until message (timer-expired timer-id _)
    :init: !! set-timer timer-id (sec * 1000.0) `relative

run-ground
  (spawn-timer-driver)

  actor
    forever (files (hash))
      during observe (file $name _)
        :init: printf "At least one reader exists for ~v\n" name
        :done: printf "No remaining readers exist for ~v\n" name
        :collect ((content (hash-ref files name #f)))
        assert (file name content)
        on message (save (file name $content)): content
        on message (delete name): #f
      on message (save (file $name $content)): hash-set files name content
      on message (delete $name): hash-remove files name

  // Shell
  let
    def e: read-bytes-line-evt (current-input-port) `any

    def (print-prompt)
      printf "> "
      (flush-output)

    def reader-count: 0
    def (generate-reader-id)
      def id: reader-count
      set! reader-count (reader-count + 1)
      id

    actor
      (print-prompt)
      until message (external-event e [? eof-object? _]) :meta-level 1
        on message (external-event e [? bytes? $bs]) :meta-level 1
            match string-split (string-trim (bytes->string/utf-8 bs))

              ["open", name]:
                def reader-id: (generate-reader-id)
                actor
                  printf "Reader ~a opening file ~v.\n" reader-id name
                  until message `(stop-watching ,name)
                    on asserted (file name $contents)
                       (printf "Reader ~a sees that ~v contains: ~v\n"
                                reader-id
                                name
                                contents)
                  printf "Reader ~a closing file ~v.\n" reader-id name

              ["close", name]:
                !! `(stop-watching ,name)

              ["write", name, words, ...]:
                !! save . file name words

              ["delete", name]:
                !! delete name

              _:
                printf "I'm afraid I didn't understand that.\n"
                printf "Try: open filename\n"
                printf "     close filename\n"
                printf "     write filename some text goes here\n"
                printf "     delete filename\n"

            sleep 0.1
            (print-prompt)
