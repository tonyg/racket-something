#lang something
// This is a copy of shell.rkt using explicit {...} blocks instead of indentation/colon-based blocks

require {
  rename-in something ('#%app' base-app)
  only-in something/lang/reader read-toplevel-syntax
  racket/system
  racket/format
  only-in racket/list flatten

  racket/port
  racket/file
  racket/string
  tabular

  for-syntax something
  for-syntax syntax/stx
}

provide {
  except-out (all-from-out something) '#%module-begin'
  rename-out ('#%plain-module-begin' '#%module-begin')

  all-from-out racket/port
  all-from-out racket/file
  all-from-out racket/string
  all-from-out tabular

  rename-out (shell-app '#%app')
  run-in-background
  pipeline
  pipe
  rev-apply
  rev-apply*
  wait
  getenv*

  read-lines
  discard
  preserve-header
  space-separated-columns
}

def-syntax shell-app stx {
  syntax-case stx [] {
    _ f arg ... {
      identifier? (syntax f) && bound-at-phase-0? (syntax f)
      syntax (base-app f arg ...)
    }
    _ f arg ... {
      identifier? (syntax f)
      build-command (syntax f) (syntax (arg ...))
    }
    _ f arg ... {
      syntax (base-app f arg ...)
    }
  }
}

begin-for-syntax {
  def bound-at-phase-0? id-stx {
    identifier-binding id-stx || \
      (procedure-arity-includes? identifier-binding 3 && identifier-binding id-stx 0 #t)
  }

  def build-command id-stx args-stx {
    with-syntax {
      command { symbol->string (syntax-e id-stx) }
      (arg ...) { args-stx }
    } {
      syntax (apply system*/exit-code (find-command (quote command))
                                      (flatten (list (parse-shell-argument arg) ...)))
    }
  }
}

def find-command command-name {
  or (find-executable-path command-name) \
     (error `find-command "No such command: ~v" command-name)
}

def-syntax parse-shell-argument stx {
  syntax-case stx [] {
    _ value {
      number? (syntax-e (syntax value))
      datum->syntax stx (number->string (syntax-e (syntax value)))
    }
    _ id {
      identifier? (syntax id) && regexp-match "^-" (symbol->string (syntax-e (syntax id)))
      datum->syntax stx (symbol->string (syntax-e (syntax id)))
    }
    _ expr {
      syntax (format-shell-argument expr)
    }
  }
}

def format-shell-argument a {
  cond {
    when string? a { a }
    when symbol? a { symbol->string a }
    when number? a { '~a' a }
    when list? a { map format-shell-argument a }
    else { error (quote format-shell-argument) "Cannot format ~v" a }
  }
}

def-operator & 8 postfix run-in-background
def-syntax run-in-background stx {
  syntax-case stx [block] {
    _ (block expr ...) {
      syntax (run-in-background (begin expr ...))
    }
    _ expr {
      syntax (shell-thread {{ expr }})
    }
  }
}

def shell-thread thunk {
  def ch { (make-channel) }
  thread {{
    with-handlers {
      when values e {
        channel-put ch e
      }
      channel-put ch (thunk)
    }
  }}
  ch
}

def-syntax ensure stx {
  syntax-case stx [block] {
    _ finally-expr (block body ...) {
      syntax (with-handlers {
        when values e {
          finally-expr
          raise e
        }
        (begin0 (begin body ...) finally-expr)
      })
    }
  }
}

def-syntax pipeline stx {
  syntax-case stx [block, '|>', '|<'] {
    _ (block) {
      syntax (copy-port (current-input-port) (current-output-port))
    }
    _ (block final-stage) {
      syntax final-stage
    }
    _ (block stage ... ('|>' final-stage)) {
      syntax (rev-apply (pipeline (block stage ...)) final-stage)
    }
    _ (block stage ... ('|>' final-stage ...)) {
      syntax (rev-apply (pipeline (block stage ...)) (final-stage ...))
    }
    _ (block stage ... ('|<' final-stage)) {
      syntax (rev-apply* (pipeline (block stage ...)) final-stage)
    }
    _ (block stage ... ('|<' final-stage ...)) {
      syntax (rev-apply* (pipeline (block stage ...)) (final-stage ...))
    }
    _ (block stage ... final-stage) {
      syntax (pipe (pipeline (block stage ...)) final-stage)
    }
  }
}

def-operator | 10 left pipe
def-syntax pipe stx {
  syntax-case stx [] {
    _ lhs rhs {
      syntax (pipe* {: run-pipe-stage lhs} {: run-pipe-stage rhs})
    }
  }
}

def run-pipe-stage result {
  cond {
    when procedure? result { (result) }
    else { result }
  }
}

def pipe* lhs-thunk rhs-thunk {
  def-values i o { (make-pipe) }
  def lhs-thread {
    parameterize [current-output-port o] {
      ensure (close-output-port o) {
        begin0 (lhs-thunk) (flush-output o)
      } &
    }
  }
  def rhs-thread {
    parameterize [current-input-port i] {
      ensure (close-input-port i) {
        (rhs-thunk)
      } &
    }
  }
  wait lhs-thread
  wait rhs-thread
}

def wait ch {
  match channel-get ch {
    ? exn? e { raise e }
    v { v }
  }
}

def-operator |> 10 left rev-apply
def-operator |< 10 left rev-apply*

def-syntax rev-apply stx {
  syntax-case stx [] {
    _ v id {
      identifier? (syntax id)
      syntax (shell-app id v)
    }
    _ v (f arg ...) {
      syntax (shell-app f arg ... v)
    }
  }
}

def-syntax rev-apply* stx {
  syntax-case stx [] {
    _ v id {
      identifier? (syntax id)
      syntax (shell-app id v)
    }
    _ v (f arg ...) {
      syntax (shell-app f v arg ...)
    }
  }
}

// Double-duty $id is an environment variable reference, and $(id) is an output capture
def-operator $ 1100 prefix getenv*
def-syntax getenv* stx {
  syntax-case stx [] {
    _ exp {
      stx-pair? (syntax exp)
      quasisyntax (pipe exp (compose string-trim port->string))
    }
    _ id {
      identifier? (syntax id)
      quasisyntax (getenv (unsyntax (symbol->string (syntax-e (syntax id)))))
    }
  }
}

current-read-interaction read-toplevel-syntax

module+ reader {
  require {
    something/lang/reader
  }
  provide {
    rename-out (read-shell-syntax read-syntax)
    read-toplevel-syntax
  }

  def read-shell-syntax src p {
    read-syntax src p :language (syntax something/shell)
  }
}

///////////////////////////////////////////////////////////////////////////
// Sketches of utilities

def read-lines (p (current-input-port)) {
  for/list ((line (in-lines p))) line
}

def (discard) {
  for ((line (in-lines))) (void)
}

def ((preserve-header nlines thunk)) {
  for ((n (in-range nlines))) (displayln (read-line))
  (thunk)
}

def space-separated-columns (converters []) {
  local-require racket/string
  def header { map string->symbol (string-split (read-line)) }
  def nsplits { length header - 1 }
  def split-once s {
    match s {
      pregexp "^\\s*(\\S+)\\s+(\\S.*)?$" [_, h, t] { values h t }
      _ { values s #f }
    }
  }
  def split-line line n converters {
    cond {
      when zero? n {
        match converters {
          [] { list line }
          cons c _ { list ((c || values) line) }
        }
      }
      else {
        let {
          def-values h t { split-once line }
          match converters {
            []       { cons                h  (split-line t (n - 1) []) }
            cons c r { cons ((c || values) h) (split-line t (n - 1) r ) }
          }
        }
      }
    }
  }
  cons header (for/list ((line (in-lines))) (split-line line nsplits converters))
}
