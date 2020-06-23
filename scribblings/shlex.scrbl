#lang scribble/manual
@require[scribble/example
         @for-label[shlex
                    racket/base
                    racket/system
                    racket/contract]]

@(define evaluator (make-base-eval))
@(evaluator '(require shlex))

@title{shlex for Racket: Simple lexical analysis}
@author[(author+email "Sorawee Porncharoenwase" "sorawee.pwase@gmail.com")]

@defmodule[shlex]

This library is a port of Python's @link["https://docs.python.org/3/library/shlex.html"]{@tt{shlex}} library,
originally implemented by Eric S. Raymond, Gustavo Niemeyer, Vinay Sajip, and
@link["https://github.com/python/cpython/commits/master/Lib/shlex.py"]{other contributors}.
The library allows users to call @racket[system]-like functions (e.g., @racket[system], @racket[process]) safely,
avoiding the @link["https://en.wikipedia.org/wiki/Code_injection#Shell_injection"]{shell injection attack}.
On the other direction, it allows users to convert arguments of @racket[system]-like functions
to a format that can be used with (safe) @racket[system*]-like functions (e.g., @racket[system*], @racket[process*]).

Note, however, that this library differs from the Python's library.
It only supports @racket[split] (@tt{shlex.split}), @racket[join] (@tt{shlex.join}),
and @racket[quote-arg] (@tt{shlex.quote}) with limited customization.
The implementation of @racket[split] is based directly on the specification of the
@link["https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html"]{Shell Command Language} in
the Open Group Base Specifications Issue 7, 2018 edition, rather than Python's implementation.

@section{Functions}

@defproc[(split [s string?] [#:comment? comment? any/c #t]) (listof string?)]{
  Split @racket[s] using shell-like syntax in POSIX mode.

  When @racket[comment?] is not @racket[#f], line comments via the character @tt{#} are supported.

  If there is an unterminated quote or escape sequence, @racket[exn:fail:read:eof] will be raised.

  The results in particular can be used with @racket[system*]-like functions.

  Notably, the function passes all but one tests in Python's test suite.
  The discrepancy is due to how Python handles comments incorrectly.

  @examples[#:eval evaluator
    (split "echo -n 'Multiple words'")
    (split "echo \"abc \\\"123\\\" def\" ghi")
    (code:line (split "ls#b") (code:comment @#,elem{Python is wrong in this example, outputting ["ls"]}))
    (split "ls #b\nsome-file")
    (split "ls #b\nsome-file" #:comment? #f)
  ]
}

@defproc[(join [xs (listof string?)]) string?]{
  Concatenate the tokens of @racket[xs]. This function is the inverse of @racket[split].
  The returned value can safely be used in a shell command line and @racket[system]-like functions.

  @examples[#:eval evaluator
    (join (list "echo" "-n" "Multiple words"))
  ]
}

@defproc[(quote-arg [s string?]) string?]{
  Return a shell-escaped version of @racket[s].
  The returned value can safely be used as one token in a shell command line and
  @racket[system]-like functions.

  @examples[#:eval evaluator
    (quote-arg "somefile; rm -rf ~")
  ]
}

@section{Safety}

It might be tempting to write code as follows:

@racketblock[
  (define (ls-unsafe arg)
    (system (format "ls ~a" arg)))
]

However, the above code has a shell injection vulnerability.
For example, when @racket[ls-unsafe] is invoked with the argument
@racket["somefile; rm -rf ~"], the argument to @racket[system]
would have the following value:

@examples[#:eval evaluator #:label #f
  (format "ls ~a" "somefile; rm -rf ~")
]

which causes the home directory to be deleted.

By using @racket[quote-arg], one can avoid this attack:

@racketblock[
  (define (ls-safe arg)
    (system (format "ls ~a" (quote-arg arg))))
]

We can see that when @racket[ls-safe] is called with @racket["somefile; rm -rf ~"],
the argument is quoted properly, thus avoiding the attack.

@examples[#:eval evaluator #:label #f
  (format "ls ~a" (quote-arg "somefile; rm -rf ~"))
]
