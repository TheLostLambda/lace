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
  (:use :cl :serapeum/bundle :llace/lazy)
  (:export :parse :@item :>>= :>> :@return :nothing :either :zero-or-more
           :one-or-more :build-parser :sat :digit :lower :upper :letter
           :alphanum :is-char :is-string))
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
returns an empty list when it isn't.
------------------------------------------------------------------------------|#

(defun parse (parser input)
  "Applies a parser function to an input string"
  (funcall parser input))

(defun @item ()
  "Parse the first character of a string"
  ;; This function returns a lambda to maintain consistent with many of the
  ;; compound parsers defined later — use `parse` to call it
  (lambda (input)
    ;; Short-circuit and return the empty list `nil` if the string is empty
    (and (string/= "" input) 
         ;; Append a new pair to the empty list `nil` with the parsed
         ;; character and the remaining substring
         (acons (char input 0) (subseq input 1) nil)))) 

;;; DIRTY WORK BELOW!
;; Need to implement: >>, >>=, return, empty, <|>, some, many

;; Need to test this on nil, single-parse, and multi-parse cases!
(deflazy >>= (parser f)
  (lambda (input)
    (mapcan
     (lambda (pair)
       (destructuring-bind (parsed . unparsed) pair
         (parse (funcall f parsed) unparsed)))
     (parse parser input))))

;; This will likely need to be made lazy at some point too!
(deflazy >> (parser-a parser-b)
  (>>= parser-a (constantly parser-b)))

;; This is the same as `return` but doesn't clash with the name
;; I should consider renaming this or getting around the package lock!
(defun @return (value)
  (lambda (input) (acons value input nil)))

;; Also a name that needs a lot of work!
(defun nothing ()
  (constantly nil))

;; This is `<|>`, but that's an illegal symbol name!
(deflazy either (parser-a parser-b)
  (lambda (input)
    (or (parse parser-a input)
        (parse parser-b input))))

;; This is `some`, but with a WIP name that doesn't clash
(defun zero-or-more (parser)
  (labels ((self (input parsed)
             (let ((result (parse parser input)))
               (if result
                   (self (cdar result) (cons (caar result) parsed))
                   (acons (reverse parsed) input nil)))))
    (lambda (input) (self input nil))))

(defun one-or-more (parser)
  (lambda (input)
    (let ((result (parse (zero-or-more parser) input)))
      (when (caar result)
          result))))

(defmacro build-parser (&body body)
  (reduce (lambda (body expr)
            (case (car expr)
              (:bind `(>>= ,(caddr expr) (lambda (,(cadr expr)) ,body)))
              (otherwise `(>> ,expr ,body))))
          (reverse body)))

;;; Derived Primitives

;; (defun sat (predicate)
;;   (>>= (@item) (lambda (char) (if (funcall predicate char) (@return char) (nothing)))))

(defun sat (predicate)
  (build-parser
   (:bind char (@item))
   (if (funcall predicate char)
       (@return char)
       (nothing))))

(defun digit () (sat #'digit-char-p))
(defun lower () (sat #'lower-case-p))
(defun upper () (sat #'upper-case-p))
(defun letter () (sat #'alpha-char-p))
(defun alphanum () (sat #'alphanumericp))

;; Ugh... Naming...
(defun is-char (char)
  (sat (lambda (c) (char-equal char c))))

(defun is-string (string)
  (if (string= "" string)
      (@return nil)
      (build-parser
        (is-char (char string 0))
        (is-string (subseq string 1))
        (@return string))))
