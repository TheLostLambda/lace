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
  (:export :item))
(in-package :llace/parsing-primatives)

#|-----------------------------------------------------------------------------
In functional parsing, a parser is a function that takes a `string` and outputs
a list of pairs, with the first element of each pair containing the parsed tree
and the second containing the remaining (unparsed) `string`.

Parsing a digit from the string "123" would yield `((#\1 . "23"))`, where `#\1`
is the parsed digit and "23" is the remaining string. Returning these parse
results as a list allows for both failure (an empty list) and ambiguity (a list
containing several partially parsed forms) to be encoded.

To start, let's write a simple parser that returns the first `character` of a
string where possible and returns an empty list when it isn't.
-----------------------------------------------------------------------------|#

(defun item (input)
  "Parse the first character of a string"
  ; Short-circuit and return the empty list `nil` if the string is empty
  (and (string/= "" input) 
       ; Append a new pair to the empty list `nil` with the parsed character
       ; and the remaining substring
       (acons (char input 0) (subseq input 1) nil))) 

;; TODO: Abandon the monads.lisp file and just add a minimal set of functions
