(defpackage llace/parsing
  (:use :cl :llace/functional-parsing)
  (:export :expr))
(in-package :llace/parsing)

;;; Just playing around

(defun nat ()
  (build-parser
    (:bind xs (one-or-more (digit)))
    (:return (parse-integer (coerce xs 'string)))))

(defun int ()
  (either (build-parser
            (is-char #\-)
            (:bind n (nat))
            (:return (- n)))
          (nat)))

;; Use whitespacep from serapeum!
(defun spacing ()
  (build-parser
    (zero-or-more (either (is-char #\Space) (is-char #\Tab)))
    (:return nil)))

;; Need to pick between `(token (int))` and `(token #'int)`
;; Make that `one-or-more` match this choice! All zero argument parsers should
;; be variables instead of functions?
(defun token (parser)
  (build-parser
    (spacing)
    (:bind token parser)
    (spacing)
    (:return token)))

(defun a-natural () (token (nat)))
(defun an-integer () (token (int)))
(defun a-character (c) (token (is-char c)))
(defun a-symbol (s) (token (is-string s)))

;;; Parsing and evaluating maths!

;; expr ::= term + expr | term
;; term ::= factor * term | factor
;; factor ::= (expr) | int

(defun expr ()
  (either (build-parser
            (:bind x (term))
            (a-character #\+)
            (:bind y (expr))
            (:return (+ x y)))
          (term)))

(defun term ()
  (either (build-parser
            (:bind x (factor))
            (a-character #\*)
            (:bind y (term))
            (:return (* x y)))
          (factor)))

(defun factor ()
  (either (build-parser
            (a-character #\()
            (:bind x (expr))
            (a-character #\))
            (:return x))
          (an-integer)))
