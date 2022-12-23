(defpackage llace/parsing
  (:use :cl :llace/functional-parsing))
(in-package :llace/parsing)

;;; Just playing around

(defun ident ()
  (build-parser
    (:bind x (lower))
    (:bind xs (zero-or-more (alphanum)))
    (:return (coerce (cons x xs) 'string))))

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
(defun is-space ()
  (build-parser
    (zero-or-more (sat (lambda (c) (char= c #\Space))))
    (:return nil)))

;; Need to pick between `(token (int))` and `(token #'int)`
;; Make that `one-or-more` match this choice! All zero argument parsers should
;; be variables instead of functions?
(defun token (parser)
  (build-parser
    (is-space)
    (:bind token parser)
    (is-space)
    (:return token)))

(defun an-identifier () (token (ident)))
(defun a-natural () (token (nat)))
(defun an-integer () (token (int)))
(defun a-symbol (s) (token (is-string s)))

;;; Parsing and evaluating maths!

;; expr ::= term + expr | term
;; term ::= factor * term | factor
;; factor ::= (expr) | int

(defun expr ()
  (either (build-parser
            (:bind x (term))
            (is-char #\+)
            (:bind y (expr))
            (:return (+ x y)))
          (term)))

(defun term ()
  (either (build-parser
            (:bind x (factor))
            (is-char #\*)
            (:bind y (term))
            (:return (* x y)))
          (factor)))

(defun factor ()
  (either (build-parser
            (is-char #\()
            (:bind x (expr))
            (is-char #\))
            (:return x))
          (int)))
