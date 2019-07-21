#lang racket/base

(provide (struct-out token)
         (struct-out namespaced-name)

         read-something-forms
         read-something-toplevel

         ;; For debugging:
         token->raw-sexp)

(require racket/match)
(require (prefix-in srfi-13: (only-in srfi/13 string-contains)))

(require (except-in parser-tools/lex token? token-value))
(require (prefix-in : parser-tools/lex-sre))

(struct token (pos kind value) #:prefab)
(struct namespaced-name (prefix id) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tokenization

(define-lex-abbrevs
  [c:hex-digit (:or numeric (:/ #\a #\f #\A #\F))]
  [c:punctuation (:or "!" "$" "%" "&"
                      "*" "+" "-" "/"
                      "<" "=" ">" "?"
                      "^" "|" "~" "@"
                      "`" ","
                      "\\")]
  [c:string-escape (:or "\\\\"
                        "\\\""
                        "\\\'"
                        "\\b"
                        "\\n"
                        "\\r"
                        "\\t"
                        "\\0"
                        (:: "\\u" c:hex-digit c:hex-digit c:hex-digit c:hex-digit)
                        (:: "\\x" c:hex-digit c:hex-digit))]
  [c:symbol-start (:or alphabetic "_" "*" "+" "-")] ;; this is getting silly
  [c:symbol-inner (:or c:symbol-start numeric c:punctuation)]
  [c:symbol-end (:or c:symbol-start numeric "!" "?")]
  [c:symbol-normal (:: c:symbol-start (:? (:: (:* c:symbol-inner) c:symbol-end)))]
  [c:symbol-quoted (:: "\'" (:* (:or c:string-escape (:~ "\\" "\'"))) "\'")]
  [c:symbol (:or c:symbol-normal c:symbol-quoted)]
  [c:namespace (:: c:symbol "::")]
  [c:operator-char (:or c:punctuation ":" ".")]
  [c:space (:or c:line-continuation (:- whitespace c:line-terminator) c:comment)]
  [c:line-continuation (:: "\\"
                           (:* (:- whitespace c:line-terminator))
                           (:or c:comment c:line-terminator))]
  [c:comment (:: "//" (:* (:~ c:line-terminator)))]
  [c:line-terminator (:or "\n" "\r" "\u2028" "\u2029")])

(define read-token
  (lexer
   [(:: (:* c:space) c:line-terminator) 'newline]
   [(:+ c:space) (read-token input-port)]
   [(:: "#lang " (:* (:~ c:line-terminator)) c:line-terminator) (read-token input-port)]
   [(:: "\"" (:* (:or c:string-escape (:~ "\\" "\""))) "\"")
    (token start-pos
           'string
           (unescape-string-lexeme lexeme))]
   [(:: (:? (:or "-" "+")) (:or "0x" "0X") (:+ c:hex-digit))
    (let ((without-0x (if (memv (string-ref lexeme 0) '(#\+ #\-))
                          (string-append (substring lexeme 0 1) (substring lexeme 3))
                          (substring lexeme 2))))
      (token start-pos
             'number
             (string->number without-0x 16)))]
   [(:: (:? (:or "-" "+")) (:+ numeric) (:? (:: "." (:+ numeric))))
    (token start-pos
           'number
           (string->number lexeme 10))]
   [(:: ":" c:symbol)
    (token start-pos
           'keyword
           (lexeme->symbol (substring lexeme 1)))]
   [(:: c:namespace (:or (:+ c:operator-char) c:symbol))
    (make-namespaced-name start-pos
                          'identifier
                          lexeme)]
   [":" (simple-token start-pos 'colon)]
   [(:or (:: (:- c:operator-char ":") (:* c:operator-char)) c:symbol)
    (token start-pos
           'identifier
           (namespaced-name #f (lexeme->symbol lexeme)))]
   ["#t" (token start-pos 'literal #t)]
   ["#f" (token start-pos 'literal #f)]
   ["(" (simple-token start-pos 'oparen)]
   [")" (simple-token start-pos 'cparen)]
   ["[" (simple-token start-pos 'obrack)]
   ["]" (simple-token start-pos 'cbrack)]
   ["{" (simple-token start-pos 'obrace)]
   ["}" (simple-token start-pos 'cbrace)]
   [";" (simple-token start-pos 'semicolon)]
   [(eof) eof]))

(define (simple-token pos kind)
  (token pos kind kind))

(define (lexeme->symbol lexeme)
  (string->symbol
   (if (char=? (string-ref lexeme 0) #\')
       (unescape-string-lexeme lexeme)
       lexeme)))

(define (make-namespaced-name pos kind lexeme)
  (define ::-index (srfi-13:string-contains lexeme "::"))
  (define prefix (substring lexeme 0 ::-index))
  (define suffix (substring lexeme (+ ::-index 2)))
  (token pos kind (namespaced-name (string->symbol prefix) (lexeme->symbol suffix))))

(define (unescape-string-lexeme lexeme)
  (unescape-escaped-string (substring lexeme 1 (- (string-length lexeme) 1))))

(define (unescape-escaped-string s)
  (list->string
   (let loop ((cs (string->list s)))
     (match cs
       ['() '()]
       [(list* #\\ #\\ rest) (cons #\\ (loop rest))]
       [(list* #\\ #\b rest) (cons #\backspace (loop rest))]
       [(list* #\\ #\n rest) (cons #\newline (loop rest))]
       [(list* #\\ #\r rest) (cons #\return (loop rest))]
       [(list* #\\ #\t rest) (cons #\tab (loop rest))]
       [(list* #\\ #\0 rest) (cons #\nul (loop rest))]
       [(list* #\\ #\u x1 x2 x3 x4 rest)
        (cons (integer->char
               (string->number (string x1 x2 x3 x4) 16))
              (loop rest))]
       [(list* #\\ #\x x1 x2 rest)
        (cons (integer->char (string->number (string x1 x2) 16))
              (loop rest))]
       [(list* #\\ x rest) (cons x (loop rest))]
       [(list* x rest) (cons x (loop rest))]))))

(define (read-something-line p #:blank-line-is-eof? [blank-line-is-eof? #f])
  (let loop ((tokens-rev '()))
    (match (read-token p)
      ['newline
       (if (null? tokens-rev)
           (if blank-line-is-eof?
               eof
               (loop '())) ;; we skip whitespace-only lines
           (reverse tokens-rev))]
      [token
       (if (eof-object? token)
           (if (null? tokens-rev)
               token
               (reverse tokens-rev))
           (loop (cons token tokens-rev)))])))

(define (read-something-lines p)
  (define line (read-something-line p))
  (if (eof-object? line)
      '()
      (cons line (read-something-lines p))))

(define (read-something-lines-toplevel p)
  (let read-more-lines ((acc '()))
    (match (read-something-line p #:blank-line-is-eof? (pair? acc))
      [(or (? eof-object?)
           (list (token _ 'semicolon 'semicolon)))
       (reverse acc)]
      [line
       (read-more-lines (cons line acc))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading

(define (closer? token)
  (memq (token-kind token) '(cparen cbrack cbrace semicolon)))

(define (non-closer? token)
  (not (closer? token)))

(define (unclosed-grouping left-pos expected-terminator)
  (flush-output) ;; TODO: remove
  (error 'something-syntax-error
         "Unclosed grouping starting at ~a: expecting ~v"
         left-pos
         expected-terminator))

(define (mismatched-close-token pos expected-terminator got)
  (flush-output) ;; TODO: remove
  (error 'something-syntax-error
         "Mismatched close-token at ~a: expecting ~v, got ~v"
         pos
         expected-terminator
         got))

(define (lines-after-semicolon tokens lines)
  (if (null? tokens)
      lines
      (cons tokens lines)))

(define (gather-lines-indented-further-than left-pos lines)
  (let loop ((acc-rev '()) (lines lines))
    (match lines
      [(cons (cons (? non-closer? t) _) _)
       #:when (> (position-col (token-pos t)) (position-col left-pos))
       (define-values (line remaining-lines) (detach-line lines))
       (loop (cons line acc-rev) remaining-lines)]
      [(cons (cons (token _ 'semicolon _) more-tokens) more-lines)
       (loop acc-rev (lines-after-semicolon more-tokens more-lines))]
      [_ (values (reverse acc-rev) lines)])))

(define (detach-line lines
                     #:left-pos [left-pos0 #f])
  (match-define (cons (and tokens (cons (token left-pos1 _ _) _)) remaining-lines) lines)
  (detach-form make-form/1 '() (or left-pos0 left-pos1) tokens remaining-lines #f))

(define (make-form pos contents)
  (token pos 'form contents))

(define (make-form/1 pos contents)
  (match contents
    [(list item) item]
    [_ (make-form pos contents)]))

(define (make-block pos contents)
  (token pos 'block contents))

(define (make-sequence pos contents)
  (token pos 'sequence contents))

(define (closer-for opener)
  (match opener
    ['oparen 'cparen]
    ['obrack 'cbrack]
    ['obrace 'cbrace]))

(define (detach-form finish-form acc-rev left-pos tokens remaining-lines grouping-terminator)
  (match tokens
    ['() #:when (not grouping-terminator)
     (match remaining-lines
       [(cons (cons (token _ 'semicolon _) _) _)
        (values (finish-form left-pos (reverse acc-rev)) remaining-lines)]
       [_
        (define-values (block-lines final-lines)
          (gather-lines-indented-further-than left-pos remaining-lines))
        (values (finish-form left-pos
                             (reverse (if (null? block-lines)
                                          acc-rev
                                          (cons (make-block left-pos block-lines) acc-rev))))
                final-lines)])]
    ['() #:when grouping-terminator
     (match remaining-lines
       ['()
        (unclosed-grouping left-pos grouping-terminator)]
       [(cons new-tokens remaining-lines)
        (detach-form finish-form acc-rev left-pos new-tokens remaining-lines grouping-terminator)])]

    [(cons (token pos 'colon _) '())
     (define-values (block-lines final-lines)
       (gather-lines-indented-further-than left-pos remaining-lines))
     (detach-form finish-form
                  (cons (make-block pos block-lines) acc-rev)
                  left-pos
                  '()
                  final-lines
                  grouping-terminator)]
    [(cons (token pos 'colon _) more-tokens) #:when (pair? more-tokens)
     (define-values (line final-lines)
       (detach-line #:left-pos left-pos (cons more-tokens remaining-lines)))
     (detach-form finish-form
                  (cons (make-block pos (list line)) acc-rev)
                  left-pos
                  '()
                  final-lines
                  grouping-terminator)]

    [(cons (token pos (and opener (or 'obrace 'obrack)) _) more-tokens)
     (define closer (closer-for opener))
     (define inner-finish-form (match opener ['obrace make-block] ['obrack make-sequence]))
     (let loop ((block-acc-rev '())
                (lines (if (null? more-tokens)
                           remaining-lines
                           (cons more-tokens remaining-lines))))
       (match lines
         ['() (unclosed-grouping left-pos closer)]
         [(cons (cons (token _ (== closer) _) final-tokens) final-lines)
          (define new-acc (cons (inner-finish-form pos (reverse block-acc-rev)) acc-rev))
          (detach-form finish-form new-acc left-pos final-tokens final-lines grouping-terminator)]
         [(cons (cons (token _ 'semicolon _) more-tokens) more-lines)
          (loop block-acc-rev (lines-after-semicolon more-tokens more-lines))]
         [(cons (cons (? closer? (token pos got _)) _) _)
          (mismatched-close-token pos closer got)]
         [_
          (define-values (line remaining-lines) (detach-line lines))
          (loop (cons line block-acc-rev) remaining-lines)]))]

    [(cons (token pos 'oparen _) more-tokens)
     (define-values (form final-lines)
       (detach-form make-form '() left-pos more-tokens remaining-lines 'cparen))
     (match final-lines
       ['() (unclosed-grouping left-pos 'cparen)]
       [(cons (cons (token _ 'cparen _) final-tokens) final-lines)
        (define new-acc (cons form acc-rev))
        (detach-form finish-form new-acc left-pos final-tokens final-lines grouping-terminator)]
       [(cons (cons (? closer? (token pos got _)) _) _)
        (mismatched-close-token pos 'cparen got)])]

    [(cons (? closer?) more-tokens)
     (values (finish-form left-pos (reverse acc-rev)) (cons tokens remaining-lines))]

    [(cons other more-tokens)
     (detach-form finish-form
                  (cons other acc-rev)
                  left-pos
                  more-tokens
                  remaining-lines
                  grouping-terminator)]))

(define (extract-forms lines)
  (match lines
    ['() '()]
    [(cons (cons (token _ 'semicolon _) more-tokens) more-lines)
     (extract-forms (lines-after-semicolon more-tokens more-lines))]
    [(cons (cons (? closer? (token pos got _)) more-tokens) more-lines)
     (error 'something-syntax-error
            "Unexpected close-token ~v at ~a"
            got
            pos)]
    [lines
     (define-values (line remaining-lines) (detach-line lines))
     (cons line (extract-forms remaining-lines))]))

(define (read-something-forms [p (current-input-port)])
  (port-count-lines! p)
  (extract-forms (read-something-lines p)))

(define (read-something-toplevel [p (current-input-port)])
  (port-count-lines! p)
  (extract-forms (read-something-lines-toplevel p)))

;; Useful for debugging.
(define (token->raw-sexp t)
  (match t
    [(token _ 'form kids) (map token->raw-sexp kids)]
    [(token _ 'block kids) (cons '#%block (map token->raw-sexp kids))]
    [(token _ 'sequence kids) (cons '#%seq (map token->raw-sexp kids))]
    [(token _ _ (namespaced-name #f id)) id]
    [(token _ _ (namespaced-name ns id)) `(#%ns ,ns ,id)]
    [(token _ 'keyword val) (string->keyword (symbol->string val))]
    [(token _ _ val) val]))

(module+ main
  (local-require racket/pretty)
  (pretty-print
   (map
    token->raw-sexp
    (read-something-forms))))
