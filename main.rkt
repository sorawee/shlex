#lang racket/base

(require racket/contract
         racket/string
         racket/port
         racket/list
         racket/match
         syntax/readerr
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (contract-out [quote-arg (-> string? string?)]
                       [split (->* (string?) (#:comment? any/c) (listof string?))]
                       [join (-> (listof string?) string?)]))


(define unsafe-pattern #rx"[^a-zA-Z0-9_@%+=:,./-]")

(define (unget s port)
  (file-position port (- (file-position port) (string-utf-8-length s))))

(define (join xs)
  (string-join (map quote-arg xs) " "))

(define (quote-arg s)
  (if (non-empty-string? s)
      (if (regexp-match unsafe-pattern s)
          (string-append "'" (string-replace s "'" "'\"'\"'") "'")
          s)
      "''"))

(define-syntax-rule (token x)
  (make-position-token x start-pos end-pos))

(define (raise-unterminated type start-pos end-pos)
  (raise-read-eof-error
   (format "split: expected ~a at line ~a column ~a"
           type
           (position-line start-pos)
           (position-col start-pos))
   #f
   (position-line start-pos)
   (position-col start-pos)
   (position-offset start-pos)
   (- (position-offset end-pos) (position-offset start-pos))))

(define (split s #:comment? [comment? #t])
  ;; See
  ;; https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html
  ;; Note that <blank> = <space> and <tab>

  (define last-double-quote-info #f)

  (define in (open-input-string s))
  (port-count-lines! in)

  (define double-quote-lex
    ;; Doc:
    ;; Enclosing characters in double-quotes ( "" ) shall preserve the literal
    ;; value of all characters within the double-quotes, with the exception
    ;; of the characters backquote, <dollar-sign>, and <backslash>, as follows:
    (lexer
     [(eof) (apply raise-unterminated
                   "a closing double quote"
                   last-double-quote-info)]
     ;; Close the double-quote
     ["\"" (lex/no-comment input-port)]

     ;; Doc:
     ;; The backslash shall retain its special meaning as an escape character
     ;; (see Escape Character (Backslash)) only when followed by
     ;; one of the following characters when considered special:
     ;; $   `   "   \   <newline>

     ;; begin group backslash
     [(:: "\\" (:or "$" "`" "\"" "\\"))
      (cons (token (substring lexeme 1)) (double-quote-lex input-port))]
     [(:: "\\" any-char)
      (cons (token lexeme) (double-quote-lex input-port))]
     ["\\" (raise-unterminated "an escape character" start-pos end-pos)]
     ;; end group backslash

     ;; Otherwise, for non-backslash
     [any-char (cons (token lexeme) (double-quote-lex input-port))]))

  (define lex/no-comment
    (lexer
     ;; Doc:
     ;; If the end of input is recognized, the current token (if any) shall
     ;; be delimited.
     [(eof) eof]

     ;; Doc:
     ;; If the current character is <backslash>, single-quote, or double-quote
     ;; and it is not quoted, it shall affect quoting for subsequent characters
     ;; up to the end of the quoted text. The rules for quoting are as described
     ;; in Quoting . During token recognition no substitutions shall be actually
     ;; performed, and the result token shall contain exactly the characters
     ;; that appear in the input (except for <newline> joining), unmodified,
     ;; including any embedded or enclosing quotes or substitution operators,
     ;; between the <quotation-mark> and the end of the quoted text.
     ;; The token shall not be delimited by the end of the quoted field.

     ;; begin group quotes
     ["\"" (begin
             (set! last-double-quote-info (list start-pos end-pos))
             (cons (token "") (double-quote-lex input-port)))]

     ;; Doc:
     ;; Enclosing characters in single-quotes ( '' ) shall preserve the literal
     ;; value of each character within the single-quotes.
     ;; A single-quote cannot occur within single-quotes.
     [(:: "'" (:* (:~ "'")) "'")
      (cons (token (substring lexeme 1 (sub1 (string-length lexeme))))
            (lex/no-comment input-port))]
     ["'" (raise-unterminated "a closing single quote" start-pos end-pos)]

     ;; Doc:
     ;; A backslash that is not quoted shall preserve the literal value of
     ;; the following character, with the exception of a <newline>.
     ;; If a <newline> follows the backslash, the shell shall interpret this
     ;; as line continuation. The backslash and <newline>s shall be removed
     ;; before splitting the input into tokens. Since the escaped <newline>
     ;; is removed entirely from the input and is not replaced by any
     ;; white space, it cannot serve as a token separator.
     ["\\\n" (lex/no-comment input-port)]

     [(:: "\\" any-char)
      (cons (token (substring lexeme 1)) (lex/no-comment input-port))]

     ["\\" (raise-unterminated "an escape character" start-pos end-pos)]
     ;; end group quotes

     ;; Doc:
     ;; If the current character is an unquoted <blank>, any token containing
     ;; the previous character is delimited and the current character shall be
     ;; discarded.
     ;;
     ;; Somehow, this specification from the previous version of the docs is
     ;; dropped, but we need it
     ;;
     ;; https://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html
     ;;
     ;; Doc:
     ;; If the current character is an unquoted <newline>,
     ;; the current token shall be delimited.
     [(:or "\n" " " "\t") '()]

     [any-char (cons (token lexeme) (lex/no-comment input-port))]))

  ;; This is to handle comments. Note that "a#b" should be lexed as "a#b",
  ;; not "a". On the other hand, "a #b" should be lexed as "a"
  (define lex/comment
    (lexer
     ;; Doc:
     ;; If the current character is a '#', it and all subsequent characters
     ;; up to, but excluding, the next <newline> shall be discarded as a
     ;; comment. The <newline> that ends the line is not considered part of
     ;; the comment.
     [(:: "#" (:* (:~ "\n"))) '()]

     [(eof) eof]

     [any-char (begin
                 (unget lexeme input-port)
                 (lex/no-comment input-port))]))

  (define (cleanup toks)
    (cond
      [(list? toks) (string-append* (map position-token-token toks))]
      [else
       ;; we have an eof at the end
       (cleanup (drop-right toks 0))]))

  (let loop ()
    (define out ((if comment? lex/comment lex/no-comment) in))
    (match out
      [(== eof) '()]
      ['() (loop)]
      [_ (cons (cleanup out) (loop))])))

(module+ main
  (split (port->string (current-input-port))))

(module+ test
  (require rackunit
           syntax/parse/define
           (for-syntax racket/base))

  (define-simple-macro (do-test {~and clause [inp outp ...]} ...)
    #:with
    (checks ...)
    (map (λ (i o c)
           (quasisyntax/loc c (check-equal? (split #,i) '#,o)))
         (attribute inp)
         (attribute outp)
         (attribute clause))

    (begin checks ...))

  (do-test
   ("x" "x")
   ("foo bar" "foo" "bar")
   (" foo bar" "foo" "bar")
   (" foo bar " "foo" "bar")
   ("foo   bar    bla     fasel" "foo" "bar" "bla" "fasel")
   ("x y  z              xxxx" "x" "y" "z" "xxxx")
   ("\\x bar" "x" "bar")
   ("\\ x bar" " x" "bar")
   ("\\ bar" " bar")
   ("foo \\x bar" "foo" "x" "bar")
   ("foo \\ x bar" "foo" " x" "bar")
   ("foo \\ bar" "foo" " bar")
   ("foo \"bar\" bla" "foo" "bar" "bla")
   ("\"foo\" \"bar\" \"bla\"" "foo" "bar" "bla")
   ("\"foo\" bar \"bla\"" "foo" "bar" "bla")
   ("\"foo\" bar bla" "foo" "bar" "bla")
   ("foo 'bar' bla" "foo" "bar" "bla")
   ("'foo' 'bar' 'bla'" "foo" "bar" "bla")
   ("'foo' bar 'bla'" "foo" "bar" "bla")
   ("'foo' bar bla" "foo" "bar" "bla")
   ("blurb foo\"bar\"bar\"fasel\" baz" "blurb" "foobarbarfasel" "baz")
   ("blurb foo'bar'bar'fasel' baz" "blurb" "foobarbarfasel" "baz")
   ("\"\"" "")
   ("''" "")
   ("foo \"\" bar" "foo" "" "bar")
   ("foo '' bar" "foo" "" "bar")
   ("foo \"\" \"\" \"\" bar" "foo" "" "" "" "bar")
   ("foo '' '' '' bar" "foo" "" "" "" "bar")
   ("\\\"" "\"")
   ("\"\\\"\"" "\"")
   ("\"foo\\ bar\"" "foo\\ bar")
   ("\"foo\\\\ bar\"" "foo\\ bar")
   ("\"foo\\\\ bar\\\"\"" "foo\\ bar\"")
   ("\"foo\\\\\" bar\\\"" "foo\\" "bar\"")
   ("\"foo\\\\ bar\\\" dfadf\"" "foo\\ bar\" dfadf")
   ("\"foo\\\\\\ bar\\\" dfadf\"" "foo\\\\ bar\" dfadf")
   ("\"foo\\\\\\x bar\\\" dfadf\"" "foo\\\\x bar\" dfadf")
   ("\"foo\\x bar\\\" dfadf\"" "foo\\x bar\" dfadf")
   ("\\'" "'")
   ("'foo\\ bar'" "foo\\ bar")
   ("'foo\\\\ bar'" "foo\\\\ bar")
   ("\"foo\\\\\\x bar\\\" df'a\\ 'df\"" "foo\\\\x bar\" df'a\\ 'df")
   ("\\\"foo" "\"foo")
   ("\\\"foo\\x" "\"foox")
   ("\"foo\\x\"" "foo\\x")
   ("\"foo\\ \"" "foo\\ ")
   ("foo\\ xx" "foo xx")
   ("foo\\ x\\x" "foo xx")
   ("foo\\ x\\x\\\"" "foo xx\"")
   ("\"foo\\ x\\x\"" "foo\\ x\\x")
   ("\"foo\\ x\\x\\\\\"" "foo\\ x\\x\\")
   ("\"foo\\ x\\x\\\\\"\"foobar\"" "foo\\ x\\x\\foobar")
   ("\"foo\\ x\\x\\\\\"\\'\"foobar\"" "foo\\ x\\x\\'foobar")
   ("\"foo\\ x\\x\\\\\"\\'\"fo'obar\"" "foo\\ x\\x\\'fo'obar")
   ("\"foo\\ x\\x\\\\\"\\'\"fo'obar\" 'don'\\''t'"
    "foo\\ x\\x\\'fo'obar"
    "don't")
   ("\"foo\\ x\\x\\\\\"\\'\"fo'obar\" 'don'\\''t' \\\\"
    "foo\\ x\\x\\'fo'obar"
    "don't"
    "\\")
   ("'foo\\ bar'" "foo\\ bar")
   ("'foo\\\\ bar'" "foo\\\\ bar")
   ("foo\\ bar" "foo bar")

   ;; Python is wrong here
   ;; ("foo#bar\\nbaz" "foo" "baz")
   
   (":-) ;-)" ":-)" ";-)")
   ("áéíóú" "áéíóú")

   ;; My tests
   ("a#b" "a#b")
   ("a #b\nc" "a" "c"))

  (check-equal? (split "a #b\nc" #:comment? #f) (list "a" "#b" "c"))

  ;; Negative tests
  (check-exn exn:fail:read:eof? (λ () (split "\\")))
  (check-exn exn:fail:read:eof? (λ () (split "\"")))
  (check-exn exn:fail:read:eof? (λ () (split "'"))))
