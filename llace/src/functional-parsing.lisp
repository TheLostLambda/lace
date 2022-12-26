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
Now that we have a few core primitives defined, we can starting thinking about
how they can be combined and repeated to form more complex parsers. We'll start
by defining some basic repetition combinators.

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
  (labels ((self (input parsed)
             ;; Attempt to parse part of the input and map over the results,
             ;; flattening into a single list of parse results
             (or (mapcan (lambda (result)
                           ;; For each parse result, recurse with the unparsed
                           ;; input `(cdr result)` as the new input and the
                           ;; parsed value `(car result)` consed to the list of
                           ;; previously parsed values
                           (self (cdr result) (cons (car result) parsed)))
                         (parse parser input))
                 ;; If there were no parse results to map over, simply return
                 ;; the collected results (reversed so they appear in the same
                 ;; order as in the input string) and the remaining input
                 (acons (reverse parsed) input nil))))
    ;; Return a lambda that wraps this `self` function, calling it with the
    ;; appropriate input and an empty initial list of parsed results
    (lambda (input) (self input nil))))

(defun @one-or-more (parser)
  "Applies PARSER to an input one or more times.

Like @ZERO-OR-MORE but fails when no values can be parsed from the input."
  ;; Construct a lambda representing the parser
  (lambda (input)
    ;; Use `@zero-or-more` to parse some number of values, storing the result
    (let ((result (parse (@zero-or-more parser) input)))
      ;; When the list of parsed values is non-empty, return the result computed
      ;; by `@zero-or-more` as-is; otherwise, the parse fails and the empty list
      ;; is returned by default.
      (when (caar result)
          result))))

;;; DIRTY WORK BELOW!
(defmacro >>= (parser f)
  `(lambda (input)
     (mapcan
      (lambda (pair)
        (destructuring-bind (parsed . unparsed) pair
          (parse (funcall ,f parsed) unparsed)))
      (parse ,parser input))))

(defmacro >> (parser-a parser-b)
  `(>>= ,parser-a (constantly ,parser-b)))

(defmacro either (&rest parsers)
  `(lambda (input)
     (or ,@(mapcar (op (list 'parse _ 'input)) parsers))))

(defmacro parser (&body body)
  (reduce (lambda (body expr)
            (case (car expr)
              (:bind `(>>= ,(caddr expr) (lambda (,(cadr expr)) ,body)))
              (otherwise `(>> ,expr ,body))))
          (reverse body)))

;;; Derived Primitives

(defun @satisfies (predicate)
  (parser
   (:bind char (@item))
   (if (funcall predicate char)
       (@return char)
       (@nothing))))

(defun @digit () (@satisfies #'digit-char-p))
(defun @lower () (@satisfies #'lower-case-p))
(defun @upper () (@satisfies #'upper-case-p))
(defun @letter () (@satisfies #'alpha-char-p))
(defun @alphanum () (@satisfies #'alphanumericp))

;; Ugh... Naming...
(defun @char (char)
  (@satisfies (lambda (c) (char-equal char c))))

(defun @string (string)
  (if (string= "" string)
      (@return nil)
      (parser
        (@char (char string 0))
        (@string (subseq string 1))
        (@return string))))
