#lang something/shell

require
  something/for


///////////////////////////////////////////////////////////////////////////
// Sketches of utilities

def space-separated-columns (converters [])
  local-require racket/string
  def header: map string->symbol (string-split (read-line))
  def nsplits: length header - 1
  def split-once s:
    match s
      pregexp "^\\s*(\\S+)\\s+(\\S.*)?$" [_, h, t]: values h t
      _: values s #f
  def split-line line n converters:
    if zero? n
       match converters
         []: list line
         cons c _: list ((c || values) line)
       let
         def-values h t: split-once line
         match converters
           []:       cons                h  (split-line t (n - 1) [])
           cons c r: cons ((c || values) h) (split-line t (n - 1) r )
  cons header (for/list { line: (in-lines) }: split-line line nsplits converters)

def ((preserve-header nlines thunk))
  for { n : in-range nlines }
    displayln (read-line)
  (thunk)

def (discard)
  for { line : (in-lines) }
    (void)

def (ignore)
  copy-port (current-input-port) (current-output-port)

///////////////////////////////////////////////////////////////////////////
// Simple demos

ls -la $HOME | grep "^d" | fgrep -v "."

ls -la | wc -l | read-line |> string-split |> car |> string->number |> \
  printf "There are ~a lines here." | sed -e "s: are : seem to be :"

pipeline
  ps -wwwax
  preserve-header 1 {: grep "racket" }
  space-separated-columns [string->number]

def message-box text:
  whiptail --title "Testing" --ok-button "OK" --msgbox text 8 50

message-box "This is pretty cool."
