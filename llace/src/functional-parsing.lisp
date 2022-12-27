#|------------------------------------------------------------------------------

                 Functional Parsing Implemented in Common Lisp
                         Authored by Brooks Rady, 2022


This literate package defines a number of parsing primitives and the ways in
which they can be combined to express choice and sequencing operations. This
code is based on the Haskell library written by Graham Hutton for parsing using
monadic combinators and has been adapted to Common Lisp for use in the `llace`
interpreter.

Haskell's built-in support for typeclasses like Functor, Applicative, Monad, and
Alternative allow a lot of useful functions and syntax to be derived from just a
few minimal definitions. Common Lisp doesn't provide the same wide range of
formal abstractions, so we'll have to write a bit more code here! The goal is
not to implement a complete set of typeclasses; it's just to define the minimal
set of functions needed to write a parser.
------------------------------------------------------------------------------|#

(defpackage llace/functional-parsing
  (:use :cl :serapeum/bundle)
  (:export :parse :@item :@return :@nothing :@zero-or-more :@one-or-more :>>=
           :>> :either :parser :@satisfies :@digit :@lower :@upper :@letter
           :@alphanum :@char :@string))
(in-package :llace/functional-parsing)

#|------------------------------------------------------------------------------
In functional parsing, a parser is a function that takes a string and outputs a
list of pairs, with the first element of each pair containing the parsed item
and the second containing the remaining (unparsed) string.

Parsing a digit from the string "123" would yield `((#\1 . "23"))`, where `#\1`
is the parsed digit and "23" is the remaining string. Returning these parse
results as a list allows for both failure (an empty list) and ambiguity (a list
containing several partially parsed forms) to be encoded.

To start, let's define `parse` as an alias for `funcall`, then write a simple
`@item` parser that returns the first character of a string where possible and
returns an empty list when it isn't. As a matter of convention, all parser names
are prefixed with `@` — this prevents their names from clashing with a number of
existing Common Lisp primitives and definitions.
------------------------------------------------------------------------------|#

(defun parse (parser input)
  "Applies a PARSER function to an INPUT string."
  (funcall parser input))

(defun @item ()
  "Parse the first character of a string."
  ;; This function returns a lambda to maintain consistent with many of the
  ;; compound parsers defined later — use `parse` to call it
  (lambda (input)
    ;; Short-circuit and return the empty list `nil` if the string is empty
    (and (string/= "" input) 
         ;; Append a new pair to the empty list `nil` with the parsed
         ;; character and the remaining substring
         (acons (char input 0) (subseq input 1) nil)))) 

#|------------------------------------------------------------------------------
Though the `@item` parser is the primary building block with which other parsers
are built, it's helpful to have a few other primitives. The first of these is
`@return`, which consumes none of its input and "parses" a constant value. This
parser "lifts" an arbitrary value into the parser monad.

An even simpler primitive is the `@nothing` parser, which fails to parse any
input and always returns an empty list.
------------------------------------------------------------------------------|#

(defun @return (value)
  "Lifts a value into the parser monad.

Returns VALUE in the parse position and propagates the input unchanged."
  ;; Construct a lambda that always returns `((value . input))`
  (lambda (input) (acons value input nil)))

(defun @nothing ()
  "A parser that always fails, returning NIL regardless of its input."
  (constantly nil))

#|------------------------------------------------------------------------------
Now that we have a few core primitives defined, we can start thinking about how
they can be repeated and combined to form more complex parsers. We'll start by
defining some basic repetition combinators.

The `@zero-or-more` parser takes another parser as an argument and repetitively
applies it to an input — greedily consuming as much input as possible and
collecting the parsed characters into a list. When the supplied parser cannot
parse anything from the input, parsing by `@zero-or-more` still succeeds but
returns an empty list (i.e. `((() . input))`).

The `@one-or-more` parser behaves exactly like `@zero-or-more` except for the
case in which nothing can be parsed from the input. In this case, instead of the
parse succeeding and returning an empty list, the whole parse fails. In terms of
regular expressions, `@zero-or-more` acts like `*` and `@one-or-more` like `+`.
------------------------------------------------------------------------------|#

(defun @zero-or-more (parser)
  "Applies PARSER to an input zero or more times.

If PARSER can parse the input given, collect and return a list of all parsed
values in the order they appear; otherwise, still return a successful parse but
containing only an empty list."
  ;; Start by defining a recursive function that will repetitively apply PARSER
  ;; to the INPUT and collect a list of parsed values in PARSED
  (labels ((self (input parsed-values)
             ;; Attempt to parse part of the input and map over the results,
             ;; flattening into a single list of parse results
             (or (mapcan
                  (op (destructuring-bind (parsed . unparsed) _
                        ;; For each parse result, recurse with the unparsed
                        ;; input and the parsed value consed to the list of
                        ;; previously parsed values
                        (self unparsed (cons parsed parsed-values))))
                  (parse parser input))
                 ;; If there were no parse results to map over, simply return
                 ;; the collected results (reversed so that they appear in the
                 ;; same order as in the input string) and the remaining input
                 (acons (reverse parsed-values) input nil))))
    ;; Return a lambda that wraps this `self` function, calling it with the
    ;; appropriate input and an empty initial list of parsed values
    (lambda (input) (self input nil))))

(defun @one-or-more (parser)
  "Applies PARSER to an input one or more times.

Like @ZERO-OR-MORE but fails when no values can be parsed from the input."
  ;; Construct a lambda representing the new parser
  (lambda (input)
    ;; Use `@zero-or-more` to parse some number of values, storing the result
    (let ((result (parse (@zero-or-more parser) input)))
      ;; When the list of parsed values is non-empty, return the result computed
      ;; by `@zero-or-more` as-is; otherwise, the parse fails and the empty list
      ;; is returned by default.
      (when (caar result)
          result))))

#|------------------------------------------------------------------------------
Now that repetition is out of the way, we need a way to chain parsers together
and work with their outputs. The bind operator — `>>=` — allows for this
chaining and is what makes these functional parsers monadic.

The `>>=` combinator takes a parser that parses some value from the input, and a
function that takes that value and returns a new parser using it. This allows
the original parser and the one returned by the passed function to be chained
together (consuming the same input) but with the second, newly constructed
parser being aware of what the first parser found. If the first parser fails,
then the second parser-constructing function is never called (since there is no
parsed value to call the function with).

Importantly, this combinator must be lazy; that is, its second function argument
should not be evaluated until it's known that the first parser succeeds. This
allows for recursive parser definitions that don't result in infinite recursion
before any input is supplied. Common Lisp, unlike Haskell, is a strict language
that always evaluates a function's arguments before it is called. To get around
this eager evaluation, `>>=` and friends are macros that do not automatically
evaluate their arguments. This serves as an analogue for true lazy evaluation
and makes the definition of self-recursive parsers possible. Though these macros
return parser lambdas, they don't adopt the standard `@` prefix to differentiate
them from true functions that can be called at runtime.

The `>>` combinator chains parsers just like `>>=`, but doesn't propagate the
first parser's return value. Instead, it simply takes two preexisting parsers,
runs them one after another, and discards the result of the first.
------------------------------------------------------------------------------|#

(defmacro >>= (parser f)
  "Applies PARSER to an input and, if parsing succeeds, calls F with the parsed
value to generate a new parser that is chained after the first.

This is a lazy version of the monadic bind operator."
  ;; Construct a quoted lambda representing the new parser
  `(lambda (input)
     ;; Attempt to parse the input and map over parse results, flattening them
     ;; into a single list
     (mapcan
      (op (destructuring-bind (parsed . unparsed) _
            ;; For each result, call f with the parsed value and apply the
            ;; resultant parser to what's left of the input
            (parse (funcall ,f parsed) unparsed)))
      (parse ,parser input))))

(defmacro >> (parser-a parser-b)
  "Chains PARSER-A and PARSER-B, discarding the output of PARSER-A.

This combinator is lazy so PARSER-B is never evaluated if parsing with PARSER-A
fails."
  ;; Reuse the definition of `>>=` for chaining, but use `constantly` to discard
  ;; the result of parser-a
  `(>>= ,parser-a (constantly ,parser-b)))

#|------------------------------------------------------------------------------
In addition to repeating and chaining parsers, it's useful to attempt several
different parses on the same input before giving up. The `either` combinator
takes any number of parsers and applies them one after the other to a particular
input. Once one of these parsing attempts succeeds, `either` short-circuits and
immediately returns the parsed value.

Like the `>>=` and `>>` combinators, `either` is lazy so its arguments aren't
evaluated unless all of the proceeding parsers have failed. This combinator is
essentially just the `or` macro wrapped in a lambda and with each of its
arguments applied to the same input by `parse`.
------------------------------------------------------------------------------|#

(defmacro either (&rest parsers)
  "Attempts parsing an input with each parser in PARSERS, returning the first
successful result.

This combinator is short-circuiting, so a parser is only run if all of the
parsers before it have failed to return a result."
  ;; Construct a quoted lambda representing the new parser
  `(lambda (input)
     ;; Map through parsers, wrapping each in a call to `parse` before splicing
     ;; the resultant list into the `or` macro for short-circuit evaluation
     (or ,@(mapcar (op (list 'parse _ 'input)) parsers))))

#|------------------------------------------------------------------------------
Fundamentally, we now have everything we need to write context-free functional
parsers, but doing so may still be tedious! To chain more than two parsers, for
example, the `>>=` and `>>` combinators need to be nested, and every additional
parser added to the chain requires another level of nesting. As you can imagine,
this results in increasingly hard-to-read code for all but the simplest parsers.

To address this nesting issue, we can define a very small DSL and write a macro
for converting this new parser language into a nested chain of `>>=` and `>>`
calls that can be executed. By default, the `parser` macro chains its `&body`
forms together using `>>`, but the special bind syntax — `(:bind foo (@item))` —
uses `>>=` to set the value of `foo` to the parsed result of `(@item)` so that
it can be recalled by parsers later in the chain. This results in a more
imperative syntax that keeps complex multi-step parsers looking clean and
abstracts away the unsightly nesting inherent to functional parsing.
------------------------------------------------------------------------------|#

(defmacro parser (&body body)
  "Define a parser chain that optionally contains :BIND statements.

The BODY forms of this macro can be either expressions returning parsers or
special :BIND statements — (:BIND FOO PARSER) — which assign the parsed value of
PARSER to FOO for the remainder of the BODY. The result returned by the BODY's
terminal form becomes the result of the entire parser chain."
  ;; The binding combinators `>>=` and `>>` chain parsers from left to right,
  ;; but `reduce` builds the parser chain from the inside out (right to
  ;; left). For the order of parsers in the body to match the order of parsers
  ;; in the resultant chain, `body` is reversed before the chain is constructed
  (reduce (lambda (chain form)
            ;; For each form in the body, check if it starts with `:bind`
            (case (first form)
              ;; If it does, then wrap the existing chain in a lambda and a call
              ;; to `>>=` that passes the value parsed by `(third form)` into
              ;; the rest of the chain
              (:bind `(>>= ,(third form) (lambda (,(second form)) ,chain)))
              ;; Otherwise, the form must be a parser and can be simply added to
              ;; the beginning of the chain
              (otherwise `(>> ,form ,chain))))
          (reverse body)))

#|------------------------------------------------------------------------------
That's it as far as primitives go! It's all of the syntax and functionality we
need to build any context-free parser. With that being said, it would be nice to
define a few more utility parsers here that will make building others easier
down the line.

The first of these derived parsers is `@satisfies`, which acts exactly like the
`@item` parser but additionally checks that the parsed character satisfies the
supplied predicate. We can then use `@satisfies` to build trivial parsers
accepting digits, lowercase letters, uppercase letters, letters in general, and
any alphanumeric character.
------------------------------------------------------------------------------|#

(defun @satisfies (predicate)
  "Parse the first character of a string if it satisfies PREDICATE."
  (parser
    ;; Parse a single character and bind it to `char`
    (:bind char (@item))
    ;; If it satisfies the predicate, return `char`; otherwise, return nothing
    ;; (a parse failure)
    (if (funcall predicate char)
        (@return char)
        (@nothing))))

(defun @digit ()
  "Parse a single digit (0–9)."
  (@satisfies #'digit-char-p))
(defun @lower ()
  "Parse a lowercase letter."
  (@satisfies #'lower-case-p))
(defun @upper ()
  "Parse an uppercase letter."
  (@satisfies #'upper-case-p))
(defun @letter ()
  "Parse any upper or lowercase letter."
  (@satisfies #'alpha-char-p))
(defun @alphanum ()
  "Parse any alphanumeric character."
  (@satisfies #'alphanumericp))

#|------------------------------------------------------------------------------
Finally it's often useful to look for particular characters or strings when
parsing keywords. The `@char` parser builds on `@satisfies` to match a single
character and `@string` builds on `@char` to match character strings.
------------------------------------------------------------------------------|#

(defun @char (char)
  "Parse a single CHAR."
  ;; Use `@satisfies` to look for a character equal to `char`
  (@satisfies (lambda (c) (char-equal char c))))

(defun @string (string)
  "Parse a STRING of characters."
  ;; Check if `string` is empty
  (if (string= "" string)
      ;; If it is, return nil (succeeding to parse, but without consuming any
      ;; input or returning anything meaningful)
      (@return nil)
      ;; Otherwise, check that the first character of `string` is present and
      ;; then recurse, using `@string` to check that the remaining characters
      ;; are also present. If they are, return the parsed string
      (parser
        (@char (char string 0))
        (@string (subseq string 1))
        (@return string))))
