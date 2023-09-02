(defpackage llace/parsing
  (:use :cl :serapeum/bundle :llace/functional-parsing)
  (:export :@expr))
(in-package :llace/parsing)

;;; Just playing around

(defun @natural ()
  (parser
    (:bind xs (@one-or-more (@digit)))
    (@return (parse-integer (coerce xs 'string)))))

(defun @integer ()
  (either (parser
            (@char #\-)
            (:bind n (@natural))
            (@return (- n)))
          (@natural)))

(defun @spacing ()
  (parser
    (@zero-or-more (either (@char #\Space) (@char #\Tab)))
    (@return nil)))

(defun @token (parser)
  (parser
    (@spacing)
    (:bind token parser)
    (@spacing)
    (@return token)))

(defun @tnatural () (@token (@natural)))
(defun @tinteger () (@token (@integer)))
(defun @tchar (c) (@token (@char c)))
(defun @tstring (s) (@token (@string s)))

;;; Parsing and evaluating maths!

;; @expr ::= @term + @expr | @term
;; @term ::= @factor * @term | @factor
;; @factor ::= (@expr) | @tinteger

(defun @expr ()
  (either (parser
            (:bind x (@term))
            (@tchar #\+)
            (:bind y (@expr))
            (@return (+ x y)))
          (@term)))

(defun @term ()
  (either (parser
            (:bind x (@factor))
            (@tchar #\*)
            (:bind y (@term))
            (@return (* x y)))
          (@factor)))

(defun @factor ()
  (either (parser
            (@tchar #\()
            (:bind x (@expr))
            (@tchar #\))
            (@return x))
          (@tinteger)))
