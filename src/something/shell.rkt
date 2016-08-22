#lang something

require
  rename-in something ('#%app' base-app)
  only-in something/lang/reader read-toplevel-syntax
  racket/system
  racket/format
  only-in racket/list flatten

  racket/port
  racket/string

  for-syntax something
  for-syntax syntax/stx

provide
  except-out (all-from-out something) '#%module-begin'
  rename-out ('#%plain-module-begin' '#%module-begin')

  all-from-out racket/port
  all-from-out racket/string

  rename-out (shell-app '#%app')
  run-in-background
  pipeline
  pipe
  rev-apply
  wait
  getenv*
  read-lines

def-syntax shell-app stx
  syntax-case stx []
    _ f arg ...
      identifier? (syntax f) && bound-at-phase-0? (syntax f)
      (syntax (base-app f arg ...))
    _ f arg ...
      identifier? (syntax f)
      build-command (syntax f) (syntax (arg ...))

begin-for-syntax
  def bound-at-phase-0? id-stx
    identifier-binding id-stx || \
      (procedure-arity-includes? identifier-binding 3 && identifier-binding id-stx 0 #t)

  def build-command id-stx args-stx
    with-syntax { command: symbol->string (syntax-e id-stx)
                  (arg ...): args-stx }
      syntax (apply system*/exit-code (find-command (quote command))
                                      (flatten (list (parse-shell-argument arg) ...)))

def find-command command-name:
  or (find-executable-path command-name) \
     (error `find-command "No such command: ~v" command-name)

def-syntax parse-shell-argument stx
  syntax-case stx []
    _ value
      number? (syntax-e (syntax value))
      datum->syntax stx (number->string (syntax-e (syntax value)))
    _ id
      identifier? (syntax id) && regexp-match "^-" (symbol->string (syntax-e (syntax id)))
      datum->syntax stx (symbol->string (syntax-e (syntax id)))
    _ expr
      (syntax (format-shell-argument expr))

def format-shell-argument a
  cond
    when string? a: a
    when symbol? a: symbol->string a
    when number? a: '~a' a
    when list? a: map a format-shell-argument
    else: error (quote format-shell-argument) "Cannot format ~v" a

def-operator & 8 postfix run-in-background
def-syntax run-in-background stx
  syntax-case stx [block]
    _ (block expr ...)
      (syntax (run-in-background (begin expr ...)))
    _ expr
      (syntax (shell-thread :: expr))

def shell-thread thunk
  def ch: (make-channel)
  thread ::
    with-handlers
      when values e:
        channel-put ch e
      channel-put ch (thunk)
  ch

def-syntax ensure stx
  syntax-case stx [block]
    _ finally-expr (block body ...)
      syntax (with-handlers:
                when values e:
                  finally-expr
                  raise e
                (begin0 (begin body ...) finally-expr))

def-syntax pipeline stx
  syntax-case stx [block]
    _ (block)
      (syntax (copy-port (current-input-port) (current-output-port)))
    _ (block final-stage)
      (syntax final-stage)
    _ (block stage0 stage ...)
      (syntax (pipe stage0 (pipeline (block stage ...))))

def-operator | 10 left pipe
def-syntax pipe stx
  syntax-case stx []
    _ lhs rhs
      (syntax (pipe* {: run-pipe-stage lhs} {: run-pipe-stage rhs}))

def run-pipe-stage result:
  if procedure? result
     (result)
     result

def pipe* lhs-thunk rhs-thunk:
  def-values i o: (make-pipe)
  def lhs-thread:
    parameterize [current-output-port o]
      (ensure (close-output-port o):
        begin0 (lhs-thunk) (flush-output o)) &
  def rhs-thread:
    parameterize [current-input-port i]
      (ensure (close-input-port i):
        (rhs-thunk)) &
  wait lhs-thread
  wait rhs-thread

def wait ch:
  match channel-get ch
    ? exn? e: raise e
    v: v

def-operator |> 10 left rev-apply
def-syntax rev-apply stx
  syntax-case stx []
    _ v id
      identifier? (syntax id)
      (syntax (id v))
    _ v (f arg ...)
      (syntax (f arg ... v))

def-operator $ 1100 prefix getenv*
def-syntax getenv* stx
  syntax-case stx []
    _ id
      (quasisyntax (getenv (unsyntax (symbol->string (syntax-e (syntax id))))))

def read-lines (p (current-input-port))
  for/list ((line (in-lines p))) line

current-read-interaction read-toplevel-syntax

module+ reader
  require
    something/lang/reader
  provide
    rename-out (read-shell-syntax read-syntax)
    read-toplevel-syntax

  def read-shell-syntax src p
    read-syntax src p :language (syntax something/shell)
