# Lexical

 - byte vectors
 - arbitrarily-spelled symbols
 - arbitrarily-spelled keywords
 - bring symbol spelling more into line with Racket orthodoxy
 - character syntax
 - other #-prefixed values
 - quote
 - syntax
 - quasiquote, unquote, unquote-splicing
 - quasisyntax, unsyntax, unsyntax-splicing

# Experiments

Current:

    symbols           'any#thing'
                      #%any#thing
    keywords          :'any#thing'
    quote             not supported
    quasiquote        not supported
    bytestrings       not supported
    characters        not supported

Smalltalk-ish:

    symbols           #'any#thing'
                      #%any#thing
    keywords          :'any#thing'
    quote             '
    quasiquote        `
    bytestrings       #"bytes"
    characters        #\c #\space
    syntax            #'
    quasisyntax       #`
    unquote           ,
    unquote-splicing  ,@
    unsyntax          #,
    unsyntax-splicing #,@

All of `quote`, `quasiquote`, `syntax` and `quasisyntax` can
automatically adapt to being used in block style, since inline or
block style is unambiguous for a single value, and they all expect
just one argument. For example, `quote` will always appear as one of

    (quote atom)
    (quote (block atom))
    (quote (compound ...))
    (quote (block (compound ...)))

Hmm. Lexical abbreviations for syntax and quasisyntax might not be
required?
