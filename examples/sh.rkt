#lang something/shell
// Simple demos

ls -la $HOME | grep "^d" | fgrep -v "."

ls -la | wc -l | read-line |> string-split |> car |> string->number |> \
  printf "There are ~a lines here." | sed -e "s: are : seem to be :"
(newline)

def ps-output
  pipeline
    ps -wwwax
    preserve-header 1 {: grep "racket" }
    space-separated-columns [string->number]
    |> csv-expr->table
print ps-output

def message-box text:
  whiptail --title "Testing" --ok-button "OK" --msgbox text 8 50

message-box "This is pretty cool."
