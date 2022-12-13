#|------------------------------------------------------------------------------

                 Functional Parsing Implemented in Common Lisp
                         Authored by Brooks Rady, 2022


This literate package defines a number of parsing primatives and the ways in
which they can be combined to express choice and sequencing operations. This
code is based on the Haskell library written by Graham Hutton for parsing using
monadic combinators and has been adapted to Common Lisp for use in the `llace`
interpreter.

Haskell's built-in support for typeclasses like Functor, Applicative, Monad, and
Alternative allow a lot of useful functions and syntax to be derived from just a
few, minimal definitions. Common Lisp doesn't provide the same wide range of
formal abstractions, so we'll have to write a bit more code here! The goal is
not to implement a complete set of typeclasses; it's just to define the minimal
set of functions needed to write a parser.
------------------------------------------------------------------------------|#

(defpackage llace/parsing-primatives
  (:use :cl)
  (:export :parse :item :<$> :pure :<*>))
(in-package :llace/parsing-primatives)

#|------------------------------------------------------------------------------
In functional parsing, a parser is a function that takes a `string` and outputs
a list of pairs, with the first element of each pair containing the parsed tree
and the second containing the remaining (unparsed) `string`.

Parsing a digit from the string "123" would yield `((#\1 . "23"))`, where `#\1`
is the parsed digit and "23" is the remaining string. Returning these parse
results as a list allows for both failure (an empty list) and ambiguity (a list
containing several partially parsed forms) to be encoded.

To start, let's define `parse` as an alias for `funcall`, then write a simple
parser that returns the first `character` of a string where possible and returns
an empty list when it isn't.
------------------------------------------------------------------------------|#

(defun parse (parser input)
  "Applies a parser function to an input string"
  (funcall parser input))

(defun item ()
  "Parse the first character of a string"
  ; This function returns a lambda to maintain consistent with many of the
  ; compound parsers defined later — use `parse` to call it
  (lambda (input)
    ; Short-circuit and return the empty list `nil` if the string is empty
    (and (string/= "" input) 
         ; Append a new pair to the empty list `nil` with the parsed character
         ; and the remaining substring
         (acons (char input 0) (subseq input 1) nil)))) 

#|------------------------------------------------------------------------------
Now we can start working our way through some minimal "typeclass" definitions —
we'll start with Functors, which allow a function to be applied to some wrapped
value. In our case, this wrapping context is that of the parser and functions
are applied to the parsed value (leaving the unparsed string untouched). The
function that provides this unwrap-apply-wrap behaviour is `fmap` or, as an
operator: `<$>`.
------------------------------------------------------------------------------|#

(defun <$> (f parser)
  "Apply a function within the context of a parser"
  ; Since our context is a function, we need to return another parser function
  (lambda (input)
    ; `mapcar` lets us apply `f` to every parse option returned by `parser` —
    ; this handles both failed parses (mapping over `nil` returns `nil`) and
    ; ambiguous parses (where there exist several parse options)
    (mapcar
     (lambda (pair)
       ; For each pair in the parse list, split into parsed and unparsed parts
       (destructuring-bind (parsed . unparsed) pair
         ; Then apply `f` to the parsed component only, reconstructing the pair
         (cons (funcall f parsed) unparsed)))
     ; Actually generate the parsed input whose parse list is mapped over
     (parse parser input))))


;;; DIRTY WORK BELOW!
;; Applicative
(defun pure (value)
  (lambda (input) (acons value input nil)))

(defun <*> (parser-f parser-x)
  (lambda (input)
    (mapcar
     (lambda (pair)
       (destructuring-bind (f . unparsed) pair
         (parse (<$> f parser-x) unparsed)))
     (parse parser-f input))))

; liftA2 f x = (<*>) (fmap f x)
;; (defun liftA2 (f parser-a parser-b)
;;   (<*> (<$> f parser-a)))
